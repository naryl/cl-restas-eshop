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
      (when-let ((user (register username password :name name :phone @phone :bonuscard @bonuscard)))
        (new-session :persistent t)
        (setf (current-user) user)))))

(restas:define-route request-user-route ("/u")
  (:decorators '@protected-anon)
  (hunchentoot:redirect "/u/orders"
                        :code hunchentoot:+http-moved-permanently+))

(restas:define-route request-user-orders-route ("/u/orders")
  (:decorators '@timer '@session '@no-cache '@protected-anon)
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
                              :curpage page)))))

(defun user-plist (user)
  (list :name (user-name user)
        ;:birthdate (awhen (user-birthdate user) (render-time +date+ it))
        :city (user-city user)
        :address (first (user-addresses user))
        :phone (user-phone user)
        :phone_valid (validated-p user 'phone)
        :email (user-email user)
        :email_valid (validated-p user 'email)))

(restas:define-route request-user-profile-route ("/u/profile")
  (:decorators '@timer '@session '@no-cache '@protected-anon)
  (soy.cabinet:personal (user-plist (current-user))))

(restas:define-route request-user-profile-route/post ("/u/profile"
                                                      :method :post)
  (:decorators '@timer '@session '@no-cache '@protected-anon)
  (with-hunchentoot-parameters (name @birthdate city address @phone @pass1 @pass2)
    (handler-case
        (eshop.odm:with-transaction
          (when (or @pass1 @pass2)
            (if (string= @pass1 @pass2)
                (eshop.odm:setobj (current-user)
                                  'pass @pass1)
                (return-from request-user-profile-route/post
                  (soy.cabinet:personal
                   (list* :error "Новые пароли не совпадают"
                          (user-plist (current-user)))))))
          (eshop.odm:setobj (current-user)
                            'phone @phone
                            'name name
                            'birthdate @birthdate
                            'city city
                            'addresses (list address)))
      (eshop.odm:validation-error (e)
        (let ((cause (slot-value e 'eshop.odm::cause))
              (msg nil))
          (if (eq (type-of cause) 'data-sift:validation-fail)
              (setf msg (data-sift:validation-fail-message cause))
              (setf msg "Какая-то ошибка в заполнении каких-то полей. "))
          (return-from request-user-profile-route/post
            (soy.cabinet:personal
             (list* :error msg
                    (user-plist (current-user))))))))
    (soy.cabinet:personal
     (list* :msg "Данные успешно обновлены"
            (user-plist (current-user))))))

;; REGISTARTION
(restas:define-route registration-page-route ("/u/registration" :method :get)
  (:decorators '@timer '@session)
  (default-page
      (soy.cabinet:registration
       (list :menu (render.menu)
             :msg (hunchentoot:get-parameter "msg")))))


(restas:define-route registration-route ("/u/registration" :method :post)
  (:decorators '@timer '@session)
  (flet ((redirect-page (msg)
            (hunchentoot:redirect (concatenate 'string "/u/registration"
                                               (prepare-get-parameters (list (cons "msg" msg))))
                                  :code hunchentoot:+http-moved-temporarily+)))
    (handler-case
        (try-to-registration)
      (account-error (e)
        (redirect-page (msg e)))
      (eshop.odm:validation-error (e)
        (let ((cause (slot-value e 'eshop.odm::cause))
              (msg nil))
          (when (eq (type-of cause) 'data-sift:validation-fail)
            (setf msg (data-sift:validation-fail-message cause)))
          (redirect-page msg))))
    (hunchentoot:redirect (aif (hunchentoot:parameter "url")
                               it
                               "/")
                          :code hunchentoot:+http-moved-temporarily+)))


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

;; LOGOUT
(restas:define-route logout-route ("/u/logout")
  (:decorators '@timer '@session '@no-cache '@protected-anon)
  (new-session)
  (hunchentoot:redirect "/"))


(restas:define-route login-page-route ("/u/login" :method :get)
  (:decorators '@timer '@session '@no-cache)
  (default-page
      (soy.cabinet:login
       (list :menu (render.menu)
             :msg (hunchentoot:get-parameter "msg")))))

(restas:define-route user-request-reset-route ("/u/reset")
  (:decorators '@timer '@session)
  (default-page
      (soy.cabinet:recovery
       (list :menu (render.menu)))))

(restas:define-route user-request-reset-route/post ("/u/reset" :method :post)
  (:decorators '@timer)
  (let ((email (hunchentoot:parameter "username")))
    (handler-case
        (let ((reset (make-password-reset email)))
          (send-reset-email reset)
          (ajax-response "error" 0 "message" "Ссылка для сброса пароля выслана вам на почту"))
      (account-error (e)
        (ajax-response "error" 1 "message" (msg e))))))

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

(restas:define-route user-apply-reset-route/post ("/u/reset/:id" :method :post)
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
        (ajax-response "error" 0 "message" "Пароль успешно сменён. Теперь вы можете авторизоваться на сайте"))
    (account-error (e)
      (ajax-response "error" 1 "message" (msg e)))))

(restas:define-route send-validation-route ("/u/send-validation" :method :post)
  (:decorators '@timer '@session '@no-cache '@protected-anon)
  "AJAX"
  (let ((slot (intern (string-upcase (hunchentoot:parameter "slot"))
                      :eshop)))
    (send-validation (make-validation (current-user) slot))
    ""))

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

;;;; Utils

(defun ajax-response (&rest kv)
  (st-json:write-json-to-string (apply #'son kv)))
