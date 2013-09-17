(defpackage eshop-test
  (:use :cl
        :lift)
  (:export #:cover
           #:run!))

(in-package eshop-test)

(defun cover ()
  (with-compilation-unit (:override t
                                    :policy '(optimize (sb-cover:store-coverage-data 3)
                                                       (safety 3) (debug 3) (speed 0)))
    (asdf:load-system :eshop :force t)))

(deftestsuite eshop-test () ())

(deftestsuite eshop-avail-test () ())

;; (run! 'eshop-test)
;; (run! 'eshop-avail-test :testsuite-initargs
;;       '(:base-url "http://dev.320-8080.ru/" :user "username" :pass "password"))

(defun run! (&optional (suite 'eshop-test) &key testsuite-initargs)
  (describe-test-result (run-tests :suite suite :testsuite-initargs testsuite-initargs) t)
  (sb-cover:report (merge-pathnames "cover/" (asdf/system:system-source-directory :eshop))))
