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


(defun try-to-registration ()
  (let ((username (hunchentoot:parameter "username"))
        (password (hunchentoot:parameter "password"))
        (name (hunchentoot:parameter "name"))
        (bonuscard nil)
        (phone nil))
    (unless (equal "" (hunchentoot:parameter "bonuscard"))
      (setf bonuscard (hunchentoot:parameter "bonuscard")))
    (unless (equal "" (hunchentoot:parameter "phone"))
      (setf phone (hunchentoot:parameter "phone")))
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
  (soy.cabinet:main (list :orders (eshop.odm:get-list 'order
                                                      :query (son 'user (current-user))))))


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


(restas:define-route registration-route ("/u/registration" :method :post)
  (:decorators '@timer '@session)
  (flet ((redirect-page (msg)
            (hunchentoot:redirect (concatenate 'string "/u/registration"
                                               (prepare-get-parameters (list (cons "msg" msg))))
                                  :code hunchentoot:+http-moved-permanently+)))
    (handler-case
        (try-to-registration)
      (account-error (e)
        (redirect-page (msg e)))
      (eshop.odm::validation-error (e)
        (let ((cause (slot-value e 'eshop.odm::cause))
              (msg nil))
          (when (eq (type-of cause) 'data-sift:validation-fail)
            (setf msg (data-sift:validation-fail-message cause)))
          (redirect-page msg))))
    (hunchentoot:redirect (aif (hunchentoot:parameter "url")
                               it
                               "/")
                          :code hunchentoot:+http-moved-permanently+)))


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

(restas:define-route loging-page-route ("/u/login" :method :get)
  (:decorators '@timer '@session)
  (default-page
      (soy.cabinet:login
       (list :menu (render.menu)
             :msg (hunchentoot:get-parameter "msg")))))

;; LOGOUT
(restas:define-route logout-route ("/u/logout")
  (new-session)
  (hunchentoot:redirect "/"
                        :code hunchentoot:+http-moved-permanently+))
