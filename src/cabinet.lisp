;;;; routes.lisp

(in-package #:eshop)

(defun prepare-get-parameters (alist)
  (format nil "?~{~A~^&~}"
          (mapcar #'(lambda (pair)
                      (concatenate 'string (car pair) "=" (hunchentoot:url-encode (cdr pair))))
                  alist)))

(defun try-to-login ()
  (with-hunchentoot-parameters (username password)
    (when (hunchentoot:parameter "log")
      (when-let ((user (login username password)))
        (new-session :persistent t)
        (setf (current-user) user)))))

(defun try-to-registration ()
  (with-hunchentoot-parameters (username password name @bonuscard @phone)
    (when (hunchentoot:parameter "reg")
      (when-let ((user (register username password name :phone @phone :bonuscard @bonuscard)))
        (new-session :persistent t)
        (setf (current-user) user)))))

(restas:define-route request-user-route ("/u")
  (:decorators (@protected "anon"))
  (hunchentoot:redirect "/u/orders"
                        :code hunchentoot:+http-moved-permanently+))

(restas:define-route request-user-orders-route ("/u/orders")
  (:decorators '@timer '@session '@no-cache (@protected "anon"))
  (let ((page-size 3))
    (let* ((query (son 'user (current-user)))
           (page (parse-integer (or (hunchentoot:get-parameter "page") "1")))
           (order-count (eshop.odm:instance-count 'order
                                                  :query query)))
      (soy.cabinet:main (list :orders (eshop.odm:get-list 'order
                                                          :query query
                                                          :sort (son 'date 1)
                                                          :skip (* (1- page) page-size)
                                                          :limit page-size)
                              :pages (iota (ceiling order-count page-size) :start 1)
                              :curpage page
                              :bonuscount (or (user-bonus (current-user)) 0))))))

(defun user-plist (user)
  (list :name (user-name user)
        ;:birthdate (awhen (user-birthdate user) (render-time +date+ it))
        :city (user-city user)
        :address (first (user-addresses user))
        :phone (user-phone user)
        :phone_valid (validated-p user 'phone)
        :email (user-email user)
        :email_valid (validated-p user 'email)
        :bonuscard (awhen (slot-value user 'bonuscard) (bonuscard-key it))
        :bonuscard_valid (string (user-bonuscard-valid-p user))
        :bonuscount (or (user-bonus user) 0)))

(restas:define-route request-user-profile-route ("/u/profile")
  (:decorators '@timer '@session '@no-cache (@protected "anon"))
  (soy.cabinet:personal (user-plist (current-user))))

;; REGISTARTION
(restas:define-route registration-page-route ("/u/registration" :method :get)
  (:decorators '@timer '@session)
  (default-page
      (soy.cabinet:registration
       (list :menu (render.menu)
             :msg (hunchentoot:get-parameter "msg")))))

;; LOGIN
(restas:define-route loging-route ("/u/login" :method :post)
  (:decorators '@timer '@session)
  (handler-case
      (try-to-login)
    (account-error (e)
      (hunchentoot:redirect (concatenate 'string "/u/login"
                                         (prepare-get-parameters (list (cons "msg" (msg e)))))
                             :code hunchentoot:+http-moved-permanently+)))
  (hunchentoot:redirect (aif (hunchentoot:parameter "url")
                             it
                             "/")
                        :code hunchentoot:+http-moved-temporarily+))

(restas:define-route login-page-route ("/u/login" :method :get)
  (:decorators '@timer '@session '@no-cache)
  (default-page
      (soy.cabinet:login
       (list :menu (render.menu)
             :msg (hunchentoot:get-parameter "msg")))))

;; LOGOUT
(restas:define-route logout-route ("/u/logout")
  (:decorators '@timer '@session '@no-cache (@protected "anon"))
  (new-session)
  (hunchentoot:redirect "/"))

(restas:define-route user-request-reset-route ("/u/reset")
  (:decorators '@timer '@session)
  (default-page
      (soy.cabinet:recovery
       (list :menu (render.menu)))))

;; EMAIL ROUTES
(restas:define-route user-apply-reset-route ("/u/reset/:id")
  (:decorators '@timer '@session)
  (let ((token (hunchentoot:parameter "token")))
    (handler-case
        (default-page
            (if (and (string/= id "")
                     (find-password-reset (parse-integer id) token))
                (soy.cabinet:newpass
                 (list :id id :token token
                       :menu (render.menu)))
                (soy.cabinet:recovery
                 (list :msg "Неправильные данные для восстановления пароля. Попробуйте ещё раз"
                       :menu (render.menu)))))
      (account-error (e)
        (default-page
            (soy.cabinet:recovery
             (list :msg (msg e)
                   :menu (render.menu))))))))

(restas:define-route validation-route ("/u/validate")
  (:decorators '@timer '@session)
  (flet ((validation-error (&optional e)
           (default-page
               (soy.cabinet:simply-message
                (list :msg (if e (msg e) "Ошибка валидации"))))))
    (handler-case
        (if-let ((id (hunchentoot:parameter "id"))
                 (token (hunchentoot:parameter "token")))
          (if-let ((user (validate-slot (parse-integer id) token)))
            (progn
              (new-session :persistent t)
              (setf (current-user) user)
              (hunchentoot:redirect "/u/profile"
                                    :code hunchentoot:+http-moved-temporarily+))
            (validation-error))
          (validation-error))
      (account-error (e) (validation-error e)))))

(restas:define-route confirm-bonuscard-route ("/u/confirm-bonuscard")
  (:decorators '@timer)
  (with-hunchentoot-parameters (id token)
    (if (confirm-bonuscard (parse-integer id) token)
        (soy.cabinet:simply-message (list :msg "Успешно"))
        (soy.cabinet:simply-message (list :msg "Ошибка")))))

(restas:define-route reject-bonuscard-route ("/u/reject-bonuscard")
  (:decorators '@timer)
  (with-hunchentoot-parameters (id token)
    (if (reject-bonuscard (parse-integer id) token)
        (soy.cabinet:simply-message (list :msg "Успешно"))
        (soy.cabinet:simply-message (list :msg "Ошибка")))))

;;;; AJAX

(define-ajax-route user-request-reset-ajax ("/u/api/reset")
  (:decorators '@timer '@no-cache)
  (let ((email (hunchentoot:parameter "username")))
    (handler-case
        (let ((reset (make-password-reset email)))
          (send-reset-email reset)
          (son "error" 0 "message" "Ссылка для сброса пароля выслана вам на почту"))
      (account-error (e)
        (son "error" 1 "message" (msg e))))))

(define-ajax-route user-apply-reset-route-ajax ("/u/api/reset/:id")
  (:decorators '@timer)
  (handler-case
      (let* ((token (hunchentoot:parameter "token"))
             (reset (find-password-reset (parse-integer id) token)))
        (unless reset
          (error 'account-error :msg "Неправильные данные для восстановления
  пароля. Попробуйте ещё раз"))
        (let ((user (password-reset-user reset)))
          (apply-password-reset reset (hunchentoot:parameter "newpass"))
          (new-session :persistent t)
          (setf (current-user) user))
        (son "error" 0 "message" "Пароль успешно сменён. Теперь вы можете авторизоваться на сайте"))
    (account-error (e)
      (son "error" 1 "message" (msg e)))))

(define-ajax-route send-validation-ajax ("/u/api/send-validation")
  (:decorators '@timer '@session '@no-cache (@protected "anon"))
  (let ((slot (intern (string-upcase (hunchentoot:parameter "slot"))
                      :eshop)))
    (send-validation (make-validation (current-user) slot))
    ""))

(define-ajax-route request-user-profile-route-ajax ("/u/api/profile")
  (:decorators '@timer '@session '@no-cache (@protected "anon"))
  (with-hunchentoot-parameters (name @birthdate city address @phone @bonuscard @pass1 @pass2)
    (handler-case
        (eshop.odm:with-transaction
          (let ((user (eshop.odm:regetobj (current-user))))
            (when (or @pass1 @pass2)
              (if (string= @pass1 @pass2)
                  (eshop.odm:setobj user 'pass @pass1)
                  (return-from request-user-profile-route-ajax
                    (son "error" 1 "message" "Новые пароли не совпадают"))))
            (unless (equal (slot-value user 'phone) @phone)
              (reset-validation user 'phone))
            (unless (equal (slot-value user 'bonuscard) @bonuscard)
              (if @bonuscard
                  (send-bonuscard-validation-mail (make-bonuscard-validation user @bonuscard))
                  (setf (slot-value user 'bonuscard) nil)))
            (eshop.odm:setobj user
                              'phone @phone
                              'name name
                              'birthdate @birthdate
                              'city city
                              'addresses (list address))
            (son "error" 0 "message" "Данные успешно обновлены")))
      (eshop.odm:validation-error (e)
        (let ((cause (slot-value e 'eshop.odm::cause))
              (msg nil))
          (if (eq (type-of cause) 'data-sift:validation-fail)
              (setf msg (data-sift:validation-fail-message cause))
              (setf msg "Какая-то ошибка в заполнении каких-то полей"))
          (son "error" 1 "message" msg))))))

(define-ajax-route registration-route ("/u/api/registration")
  (:decorators '@timer '@session)
  (handler-case
      (progn
        (try-to-registration)
        (son "error" 0))
    (account-error (e)
      (son "error" 1 "message" (msg e)))
    (eshop.odm:validation-error (e)
      (let ((cause (slot-value e 'eshop.odm::cause))
            (msg nil))
        (when (eq (type-of cause) 'data-sift:validation-fail)
          (setf msg (data-sift:validation-fail-message cause)))
        (son "error" 1 "message" msg)))))
