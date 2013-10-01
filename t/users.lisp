
(in-package eshop-test)

(deftestsuite users (eshop-test)
  ()
  (:run-setup :once-per-suite)
  (:setup (eshop.odm:connect "zifry-test")
          (dolist (coll (mapcar #'eshop.odm::symbol-fqn
                                '(eshop::user)))
            (mongo:drop-collection eshop.odm::*db* coll)
            (function-cache:clear-cache-all-function-caches))))

(addtest timeout
  (ensure (eq nil (eshop::timeout-p (get-universal-time) 1)))
  (ensure (eq t (eshop::timeout-p (- (get-universal-time) 2) 1))))

(addtest register
  (let ((obj (eshop::register "login" "password")))
    (ensure-error (eshop::register "login" "password"))
    (eshop.odm:remobj obj)))

(addtest login
  (let ((obj (eshop::register "login" "password")))
    (eshop::login "login" "password")
    (ensure-error (eshop::login "logn" "password"))
    (ensure-error (eshop::login "login" "passwrd"))
    (eshop.odm:remobj obj)))

(addtest expired-account)

(addtest expired-reset)

(addtest validation)

(addtest emails)

(addtest recovery
  (ensure-error (eshop::make-password-reset "ololo"))
  (let* ((user (eshop::register "login" "password"))
         (reset (eshop::make-password-reset "login")))
    (ensure-error
      (eshop::apply-password-reset (eshop.odm:serializable-object-key reset)
                                   "ololo"
                                   "newpass"))
    (ensure-error
      (eshop::apply-password-reset "ololo"
                                   (eshop::password-reset-token reset)
                                   "newpass"))
    (eshop::apply-password-reset (eshop.odm:serializable-object-key reset)
                                 (eshop::password-reset-token reset)
                                 "newpass")
    (eshop::login "login" "newpass")
    (eshop.odm:remobj user)))
