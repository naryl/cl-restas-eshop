;;;; routes.lisp

(in-package #:eshop)

(restas:define-route request-user-route ("/user")
  (hunchentoot:redirect "/user/profile"
                        :code hunchentoot:+http-moved-permanently+))



(restas:define-route request-user-orders-route ("/user/orders")
  (:decorators '@timer '@session '@no-cache)
  (soy.cabinet:main))


(restas:define-route request-user-profile-route ("/user/profile")
  (:decorators '@timer '@session '@no-cache)
  (soy.cabinet:personal))