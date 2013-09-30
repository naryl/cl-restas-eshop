
(in-package eshop)

(defun password-reset-timeout ()
  (* 24 60 60))

(defun user-validation-timeout ()
  (* 24 60 60))

;;;; Classes

(defclass user (eshop.odm:persistent-object)
  ((pass :type (or null string)
         :serializable t
         :accessor user-pass
         :initarg :pass)
   (phone :type (or null string)
          :serializable t
          :accessor user-phone
          :initarg :phone)
   (addresses :type list
              :serializable t
              :accessor user-addresses)
   (created :type number
            :serializable t
            :accessor user-created
            :initform (get-universal-time))
   (validation :type (or boolean string)
               :serializable t
               :reader user-validation
               :initform (create-random-string 36 36)))
  (:metaclass eshop.odm:persistent-class))

(defun user-email (user)
  (eshop.odm:serializable-object-key user))

(defun validated-p (user)
  (eq (user-validation user) t))

(defclass password-reset (eshop.odm:persistent-object)
  ((user :type user
         :serializable t
         :accessor password-reset-user
         :initarg :user)
   (token :type string
          :serializable t
          :accessor password-reset-token
          :initarg :token)
   (timestamp :type number
              :serializable t
              :reader password-reset-timestamp
              :initform (get-universal-time)))
  (:metaclass eshop.odm:persistent-class))

(defclass order (eshop.odm:persistent-object)
  ((phone :type string
          :serializable t
          :accessor order-phone
          :initarg :phone)
   (email :type string
          :serializable t
          :accessor order-email
          :initarg :email)
   (user :type user
         :serializable t
         :accessor order-user
         :initform nil
         :initarg :user)
   (date :type number
         :serializable t
         :reader order-date
         :initform (get-universal-time)
         :initarg :date)
   (address :type address
            :serializable t
            :accessor order-address
            :initarg :address)
   (username :type string
             :serializable t
             :accessor order-username
             :initarg :username)
   (userfamily :type string
               :serializable t
               :accessor order-family
               :initarg :userfamily)
   (ekk :type string
        :serializable t
        :accessor order-ekk
        :initarg :ekk)
   (bonuscount :type number
               :serializable t
               :accessor order-bonuscount
               :initarg :bonuscount)
   (comment :type string
            :serializable t
            :accessor order-comment
            :initarg :comment)
   (items :type list
          :serializable t
          :accessor order-items
          :initarg :items)
   (delivery :type number
             :serializable t
             :accessor order-delivery
             :initarg :delivery)
   (order-state :type number
                :initform 0
                :initarg :state
                :accessor order-state
                :serializable t))
  (:metaclass eshop.odm:persistent-class))

(defun order-total (order)
  (+ (order-delivery order)
     (reduce #'+ (order-items order)
             :key #'(lambda (order-item)
                      (* (order-item-price order-item)
                         (order-item-count order-item))))))

(defclass address (eshop.odm:serializable-object)
  ((city :type string
         :serializable t
         :accessor address-city
         :initarg :city)
   (address :type string
            :serializable t
            :accessor address-address
            :initarg :address))
  (:metaclass eshop.odm:serializable-class))

(defclass order-item (eshop.odm:serializable-object)
  ((article :type string
            :serializable t
            :accessor order-item-article
            :initarg :article)
   (name :type string
         :serializable t
         :accessor order-item-name
         :initarg :name)
   (price :type number
          :serializable t
          :accessor order-item-price
          :initarg :price)
   (site-price :type number
               :serializable t
               :accessor order-item-site-price
               :initarg :site-price)
   (bonuscount :type number
               :serializable t
               :accessor order-item-bonuscount
               :initarg :bonuscount)
   (count :type number
          :serializable t
          :accessor order-item-count
          :initarg :count))
  (:metaclass eshop.odm:serializable-class))

;;;; API

(define-condition account-error (error)
  ())

(defun login (email password)
  "Create a new session with login data for user
identified by EMAIL and PASSWORD if one exists. Otherwise throw ACCOUNT-ERROR"
  (let ((user (eshop.odm:getobj 'user email)))
    (if user
        (if-let ((user-pass (user-pass user)))
          (equal password user-pass)
          (error 'account-error))
        (error 'account-error))
    (eshop.odm:with-transaction
      (setf (session-user (new-session :persistent t))
            user))))

(defun register (email password)
  "Create a new user with EMAIL and PASSWORD if one doesn't exist.
Otherwise throw ACCOUNT-ERROR"
  (clean-accounts)
  (eshop.odm:with-transaction
    (when (eshop.odm:getobj 'user email)
      (error 'account-error))
    (let ((session (new-session :persistent t))
          (user (make-instance 'user
                               :key email
                               :pass password)))
      (send-validation-email user)
      (setf (session-user session)
            user))))

(defun logout ()
  "Create a new session without any login data"
  (new-session))

(defun make-password-reset (email)
  "Creates a password-rest object and sends its data to user's email"
  (clean-tokens)
  (if-let ((user (eshop.odm:getobj 'user email)))
    (send-reset-email
     (make-instance 'password-reset
                    :user user
                    :token (create-random-string 36 36)))
    (error 'account-error)))

(defun apply-password-reset (id token new-password)
  "Resets a user's password using data created by MAKE-PASSWORD-RESET"
  (if-let ((reset (eshop.odm:getobj 'password-reset id)))
    (if (equal token (password-reset-token reset))
        (eshop.odm:with-transaction
          (let ((user (password-reset-user reset)))
            (eshop.odm:remobj reset)
            (setf (user-pass user) new-password)))
        (error 'account-error))
    (error 'account-error)))

(defun validate-user (id token)
  (if-let ((user (eshop.odm:getobj 'user id)))
    (if (equal token (user-validation user))
        (eshop.odm:setobj user
                          'validation t)
        (error 'account-error))
    (error 'account-error)))

(defun send-reset-email (reset)
  ;; TODO: send proper email
  (let* ((user (password-reset-user reset))
         (mail (user-email user))

         (body (format nil "http://localhost:4246/user-recover?reset=~A&token=~A"
                       (eshop.odm:serializable-object-key reset) (password-reset-token reset))))
    (sendmail:send-email :to mail
                         :body body)))

(defun send-validation-email (user)
  ;; TODO: send proper email
  (let* ((mail (user-email user))
         (body (format nil "http://localhost:4246/user-valiadate?user=~A&token=~A"
                       (eshop.odm:serializable-object-key user) (user-validation user))))
    (sendmail:send-email :to mail
                         :body body)))

(defun clean-tokens ()
  "Remove stale tokens from the database"
  (eshop.odm:doobj (reset 'password-reset)
    (when (timeout-p (password-reset-timestamp reset)
                     (password-reset-timeout))
      (eshop.odm:remobj reset))))

(defun clean-accounts ()
  (let ((non-validated-users (eshop.odm:get-list 'user
                                                 (son 'validation
                                                      (son "$nin"
                                                           '("true" "false"))))))
    (dolist (user non-validated-users)
      (when (timeout-p (user-created user)
                       (user-validation-timeout))
        (eshop.odm:setobj user
                          'validation nil
                          'password nil)))))

(defun timeout-p (created timeout)
  (> (+ created timeout)
     (get-universal-time)))
