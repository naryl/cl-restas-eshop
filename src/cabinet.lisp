;;;; routes.lisp

(in-package #:eshop)

(restas:define-route request-user-route ("/user" :decorators '(@timer @session @no-cache))
  (soy.cabinet:main))
