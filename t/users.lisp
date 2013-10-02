
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

(addtest expired-account
  (eshop.odm:with-transaction
    (let ((user (eshop::register "login" "password")))
      (setf (slot-value user 'eshop::created)
            (- (slot-value user 'eshop::created)
               (eshop::user-validation-timeout)
               10))))
  (eshop::clean-accounts)
  (ensure-error (eshop::login "login" "password"))
  (eshop.odm:remobj (eshop.odm:getobj 'eshop::user "login")))

(addtest expired-recovery
  (ensure-error (eshop::make-password-reset "invalid"))
  (let ((user (eshop::register "login" "password")))
    (let ((reset (eshop::make-password-reset "login")))
      (eshop.odm:setobj reset
                        'eshop::timestamp
                        (- (eshop::password-reset-timestamp reset)
                           (eshop::password-reset-timeout)
                           10))
      (eshop::clean-tokens)
      (ensure-error
        (eshop::apply-password-reset (eshop.odm:serializable-object-key reset)
                                     (eshop::password-reset-token reset)
                                     "newpass"))
      (mapc #'eshop.odm:remobj (list user reset)))))

(addtest validation
  (let ((user (eshop::register "login" "password")))
    (ensure-error (eshop::validate-user "logn"
                                        (eshop::user-validation-token user)))
    (ensure-error (eshop::validate-user "login"
                                        "invalid"))
    (eshop::validate-user "login"
                          (eshop::user-validation-token user))
    (eshop.odm::setobj user 'eshop::created
                       (- (slot-value user 'eshop::created)
                          (eshop::user-validation-timeout)
                          10))
    (eshop::clean-accounts)
    (ensure (eshop::login "login" "password"))
    (eshop.odm:remobj user)))

(addtest emails
  (let* ((user (eshop::register "login" "password"))
         (reset (eshop::make-password-reset "login")))
    (eshop::send-validation-email user)
    (eshop::send-reset-email reset)
    (mapc #'eshop.odm:remobj (list user reset))))

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
