;;;; routes.lisp

(in-package #:eshop)

(restas:define-route request-user-route ("/user")
  (hunchentoot:redirect "/user/profile"
                        :code hunchentoot:+http-moved-permanently+))



(restas:define-route request-user-orders-route ("/user/orders")
  (:decorators '@timer '@session '@no-cache)
  (soy.cabinet:main (list :orders (eshop.odm:get-list 'order
                                                      :query (son 'user (current-user))))))


(restas:define-route request-user-profile-route ("/user/profile")
  (:decorators '@timer '@session '@no-cache)
  (soy.cabinet:personal))


;; LOGIN
(restas:define-route loging-route ("user/login" :method :post)
  (:decorators '@timer '@session)
  (hunchentoot:redirect (aif (hunchentoot:parameter "url")
                             it
                             "/")
                        :code hunchentoot:+http-moved-permanently+))

;; LOGOUT
(restas:define-route logout-route ("user/logout")
  (new-session)
  (hunchentoot:redirect "/"
                        :code hunchentoot:+http-moved-permanently+))


(restas:define-route loging-page-route ("user/login" :method :get)
  (:decorators '@timer '@session)
  (default-page
      (soy.cabinet:login
       (list :menu (render.menu)))))
