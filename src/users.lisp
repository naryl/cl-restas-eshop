
(in-package eshop)

(defclass user (eshop.odm:persistent-object)
  ((pass :type string
         :serializable t
         :accessor user-pass
         :initarg :pass)
   (phone :type (or null string)
          :serializable t
          :accessor user-phone
          :initarg :phone)
   (addresses :type list
              :serializable t
              :accessor user-addresses))
  (:metaclass eshop.odm:persistent-class))

(defclass order (eshop.odm:persistent-object)
  ((phone :type string
          :serializable t
          :accessor order-phone
          :initarg :phone)
   (user :type user
         :serializable t
         :accessor order-user
         :initarg :user)
   (address :type address
            :serializable t
            :accessor order-address
            :initarg :address)
   (items :type list
          :serializable t
          :accessor order-items
          :initarg :address)
   (state :type string
          :serializable t
          :accessor order-state
          :initarg :state))
  (:metaclass eshop.odm:persistent-class))

(defclass address (eshop.odm:serializable-object)
  ((city :type string
         :serializable t)
   (street :type string
           :serializable t)
   (building :type string
             :serializable t))
  (:metaclass eshop.odm:serializable-class))

(defclass order-item (eshop.odm:serializable-object)
  ((article :type string
            :serializable t
            :accessor order-item-article)
   (name :type string
         :serializable t
         :accessor order-item-name)
   (price :type number
          :serializable t
          :accessor order-item-price))
  (:metaclass eshop.odm:serializable-class))

(define-condition account-error (error)
  ())

(defun login (username password)
  (let ((user (eshop.odm:getobj 'user username)))
    (unless (and user
                 (equal password (user-pass user)))
      (error 'account-error))
    (eshop.odm:with-transaction
      (setf (session-user (new-session :persistent t))
            user))))

(defun register (username password)
  (eshop.odm:with-transaction
    (when (eshop.odm:getobj 'user username)
      (error 'account-error))
    (let ((session (new-session :persistent t))
          (user (make-instance 'user
                               :key username
                               :pass password)))
      (setf (session-user session)
            user))))

(defun logout ()
  (new-session))
