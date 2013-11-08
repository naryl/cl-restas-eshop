
(in-package eshop-test)

(deftestsuite users (eshop-test)
  ()
  (:run-setup :once-per-suite)
  (:setup (eshop.odm:connect "zifry-test")
          (dolist (coll (mapcar #'eshop.odm::symbol-fqn
                                '(eshop::user)))
            (mongo:drop-collection eshop.odm::*db* coll)
            (function-cache:clear-cache-all-function-caches))
          (eshop.odm:connect "zifry-test")))

(addtest timeout
  (ensure (eq nil (eshop::timeout-p (get-universal-time) 1)))
  (ensure (eq t (eshop::timeout-p (- (get-universal-time) 2) 1))))

(addtest register
  (let ((obj (eshop::register "test@test.ru" "password" "test test")))
    (ensure-error (eshop::register "test@test.ru" "password" "test test"))
    (eshop.odm:remobj obj)))

(addtest login
  (let ((obj (eshop::register "test@test.ru" "password" "test test")))
    (eshop::login "test@test.ru" "password")
    (ensure-error (eshop::login "logn" "password"))
    (ensure-error (eshop::login "test@test.ru" "passwrd"))
    (eshop.odm:remobj obj)))

(addtest expired-recovery
  (ensure-error (eshop::make-password-reset "invalid"))
  (let ((user (eshop::register "test@test.ru" "password" "test test")))
    (let ((reset (eshop::make-password-reset "test@test.ru")))
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
      (eshop.odm:remobj user reset))))

(addtest validation
  (let* ((user (eshop::register "test@test.ru" "password" "test test"))
         (validation (eshop::make-validation user 'eshop::email)))
    (eshop::validate-slot (eshop::validation-key validation)
                          (eshop::validation-token validation))
    (ensure (eshop::validated-p (eshop.odm:regetobj user) 'eshop::email))
    (eshop.odm:remobj user)))

(addtest emails
  (let* ((user (eshop::register "test@test.ru" "password" "test test"))
         (reset (eshop::make-password-reset "test@test.ru")))
    (eshop::send-validation (eshop::make-validation user 'eshop::email) :domain "localhost")
    (eshop::send-reset-email reset :domain "localhost")
    (eshop.odm:remobj user reset)))

(addtest recovery
  (ensure-error (eshop::make-password-reset "ololo"))
  (let* ((user (eshop::register "test@test.ru" "password" "test test"))
         (reset (eshop::make-password-reset "test@test.ru")))
    (ensure-error
      (eshop::apply-password-reset (eshop.odm:serializable-object-key reset)
                                   "ololo"
                                   "newpass"))
    (ensure-error
      (eshop::apply-password-reset "ololo"
                                   (eshop::password-reset-token reset)
                                   "newpass"))
    (eshop::apply-password-reset reset
                                 "newpass")
    (eshop::login "test@test.ru" "newpass")
    (eshop.odm:remobj user)))
