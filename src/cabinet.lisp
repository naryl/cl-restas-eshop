;;;; routes.lisp

(in-package #:eshop)


(defun prepare-get-parameters (alist)
  (format nil "?~{~A~^&~}"
          (mapcar #'(lambda (pair)
                      (concatenate 'string (car pair) "=" (hunchentoot:url-encode (cdr pair))))
                  alist)))


(defun try-to-login ()
  (let ((username (hunchentoot:parameter "username"))
        (password (hunchentoot:parameter "password")))
    (when (hunchentoot:parameter "log")
      (when-let ((user (login username password)))
        (new-session :persistent t)
        (setf (current-user) user)))))

(defmacro validation-username (value)
  `(funcall (data-sift:compile-parse-rule 'data-sift:email :message "Ошибка при вводе email") ,value))

(defmacro validation-password (value)
  `(funcall (data-sift:compile-parse-rule 'string :min-length 6 :max-length 30 :message "Ошибка при вводе пароля") ,value))

(defmacro validation-name (value)
  `(funcall (data-sift:compile-parse-rule 'string :min-length 3 :max-length 30 :message "Ошибка при вводе имени") ,value))

(defmacro validation-bonuscard (value)
  `(funcall (data-sift:compile-parse-rule 'string :min-length 3 :max-length 30 :message "Ошибка при вводе номера бонусной карты") ,value))

(defmacro validation-phone (value)
  `(funcall (data-sift:compile-parse-rule 'data-sift:regexp
                                          :regex "^8+([0-9]{9})$"
                                          :message "Ошибка при вводе номера телефона") ,value))

(defun try-to-registration ()
  (let ((username (validation-username (hunchentoot:parameter "username")))
        (password (validation-password (hunchentoot:parameter "password")))
        (name (validation-name (hunchentoot:parameter "name")))
        (bonuscard (validation-bonuscard (hunchentoot:parameter "bonuscard")))
        (phone (validation-phone (hunchentoot:parameter "phone"))))
    (when (hunchentoot:parameter "reg")
      (when-let ((user (register username password :name name :phone phone :bonuscard bonuscard)))
        (send-validation-email user)
        (new-session :persistent t)
        (setf (current-user) user)))))

;;
(restas:define-route request-user-route ("/u")
    (:decorators '@protected-anon)
    (hunchentoot:redirect "/u/profile"
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


(restas:define-route request-user-profile-route ("/u/profile")
  (:decorators '@timer '@session '@no-cache '@protected-anon)
  (soy.cabinet:personal))


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
                        :code hunchentoot:+http-moved-permanently+))

;; LOGOUT
(restas:define-route logout-route ("/u/logout")
  (new-session)
  (hunchentoot:redirect "/"
                        :code hunchentoot:+http-moved-permanently+))


(restas:define-route login-page-route ("/u/login" :method :get)
  (:decorators '@timer '@session)
  (default-page
      (soy.cabinet:login
       (list :menu (render.menu)
             :msg (hunchentoot:get-parameter "msg")))))
