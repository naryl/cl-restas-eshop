
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
   (validation :type string
               :serializable t
               :accessor user-validation
               :initform "PENDING")
   (validation-token :type string
                     :serializable t
                     :reader user-validation-token
                     :initform (create-random-string 36 36)))
  (:metaclass eshop.odm:persistent-class))

(defun user-email (user)
  (eshop.odm:serializable-object-key user))

(defclass password-reset (eshop.odm:persistent-object)
  ((user :serializable t
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
  ((msg :type string
        :initarg :msg
        :reader msg)))

(defun login (email password)
  "Check if the login data is correct and throw ACCOUNT-ERROR if not"
  (if-let ((user (eshop.odm:getobj 'user email)))
    (if-let ((user-pass (user-pass user)))
      (if (equal password user-pass)
          user
          (error 'account-error :msg "Неправильный логин или пароль"))
      (error 'account-error :msg "Учётная запись не была подтверждена вовремя. Попробуйте восстановить пароль."))
    (error 'account-error :msg "Неправильный логин или пароль")))

(defun register (email password)
  "Create a new user with EMAIL and PASSWORD if one doesn't exist.
Otherwise throw ACCOUNT-ERROR"
  (clean-accounts)
  (when (eshop.odm:getobj 'user email)
    (error 'account-error "Такой пользователь уже есть"))
  (make-instance 'user
                 :key email
                 :pass password))

(defun make-password-reset (email)
  "Creates a password-reset object and sends its data to user's email"
  (clean-tokens)
  (if-let ((user (eshop.odm:getobj 'user email)))
    (make-instance 'password-reset
                   :user user
                   :token (create-random-string 36 36))
    (error 'account-error :msg "На эту почту не зарегистрирован аккаунт")))

(defun apply-password-reset (id token new-password)
  "Resets a user's password using data created by MAKE-PASSWORD-RESET"
  (if-let ((reset (eshop.odm:getobj 'password-reset id)))
    (if (equal token (password-reset-token reset))
        (eshop.odm:with-transaction
          (let ((user (password-reset-user reset)))
            (eshop.odm:remobj reset)
            (setf (user-pass user) new-password)))
        (error 'account-error :msg  "Неправильные данные для сброса пароля"))
    (error 'account-error :msg "Неправильные данные для сброса пароля")))

(defun validate-user (id token)
  (if-let ((user (eshop.odm:getobj 'user id)))
    (if (and (equal (user-validation user)
                    "PENDING")
             (equal token (user-validation-token user)))
        (eshop.odm:setobj user
                          'validation "DONE"
                          'validation-token "")
        (error 'account-error :msg "Неправильные данные для валидации учётной записи"))
    (error 'account-error :msg "Неправильные данные для валидации учётной записи")))

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
  (let ((non-validated-users (eshop.odm::get-list 'user
                                                  (son 'validation "PENDING"))))
    (dolist (user non-validated-users)
      (when (timeout-p (user-created user)
                       (user-validation-timeout))
        (eshop.odm:setobj user
                          'validation "EXPIRED"
                          'validation-token ""
                          'pass nil)))))

(defun timeout-p (created timeout)
  (> (get-universal-time)
     (+ created timeout)))

;;;; Require hunchentoot context

(defun current-user ()
  (session-user (start-session)))

(defun (setf current-user) (user)
  (eshop.odm:with-transaction
    (setf (session-user (start-session))
          user)))
