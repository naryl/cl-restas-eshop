
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-cover))

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

(defun run! (&optional (suite 'eshop-test))
  (describe-test-result (run-tests :suite suite) t)
  (sb-cover:report (merge-pathnames "cover/" (asdf/system:system-source-directory :eshop))))
