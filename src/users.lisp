
(in-package eshop)

(defun password-reset-timeout ()
  (* 24 60 60))

(defun user-validation-timeout ()
  (* 24 60 60))

(defun %compile-nullable-validation-rule (parse-rule)
  #'(lambda (value)
      (or (null value)
          (funcall parse-rule value))))

;;;; Classes

(defclass user (eshop.odm:persistent-object)
  ((eshop.odm:key :validation (data-sift:compile-parse-rule 'data-sift:email
                                                             :message "Ошибка при вводе email"))
   (pass :type (or null string)
         :serializable t
         :accessor user-pass
         :validation (%compile-nullable-validation-rule
                      (data-sift:compile-parse-rule 'string
                                                    :min-length 6 :max-length 100
                                                    :message "Пароль должен быть не короче шести символов"))
         :initarg :pass)
   (name :type string
         :serializable t
         :accessor user-name
         :validation (data-sift:compile-parse-rule 'string
                                                   :min-length 3 :max-length 30
                                                   :message "Ошибка при вводе имени")
         :initform ""
         :initarg :name)
   (phone :type (or null string)
          :serializable t
          :accessor user-phone
          :validation (%compile-nullable-validation-rule
                       (data-sift:compile-parse-rule 'data-sift:regexp
                                                     :regex "^8([0-9]{10})$"
                                                     :message "Введите телефон в формате 8123456789"))
          :initform nil
          :initarg :phone)
   (bonuscard :type (or null bonuscard)
              :serializable t
              :accessor user-bonuscard
              :initarg :bonuscard)
   (addresses :type list
              :serializable t
              :initform nil
              :accessor user-addresses)
   (created :type number
            :serializable t
            :reader user-created
            :initform (get-universal-time))
   (birthdate :type (or null number)
              :serializable t
              :accessor user-birthdate
              :initform nil)
   (city :type (or null string)
         :serializable t
         :accessor user-city
         :initform nil)
   (roles :type list
          :serializable t
          :accessor user-roles
          :initform (list "anon"))
   (validations :type list
                :serializable t
                :initform nil))
  (:metaclass eshop.odm:persistent-class))

(defmethod print-object ((obj user) stream)
  (when (slot-boundp obj 'roles)
    (format stream "~A" (user-roles obj))))

(defun user-email (user)
  (eshop.odm:serializable-object-key user))

(defparameter +bonuscard-validator+
  (data-sift:compile-parse-rule 'string
                                :min-length 3 :max-length 30
                                :message "Ошибка при вводе номера бонусной карты"))

(defmethod initialize-instance ((user user) &rest args &key bonuscard)
  (remf args :bonuscard)
  (apply #'call-next-method user args)
  (when bonuscard
    (funcall +bonuscard-validator+ bonuscard)
    (setf (slot-value user 'bonuscard)
          (or (eshop.odm:getobj 'bonuscard bonuscard)
              (make-instance 'bonuscard
                             :key bonuscard)))))

(defclass bonuscard (eshop.odm:persistent-object)
  ((eshop.odm:key :validation +bonuscard-validator+)
   (count :type fixnum
          :initform 0
          :initarg :count
          :serializable t
          :accessor bonuscard-count))
  (:metaclass eshop.odm:persistent-class))

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
   (bonuscard :type string
              :serializable t
              :accessor order-bonuscard
              :initarg :bonuscard)
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

(defclass validation (eshop.odm:persistent-object)
  ((object :type eshop.odm:persistent-object
           :serializable t
           :reader validation-object
           :initarg :object)
   (slot :type symbol
         :serializable t
         :reader validation-slot
         :initarg :slot)
   (token :type string
          :serializable t
          :reader validation-token
          :initarg :token))
  (:metaclass eshop.odm:persistent-class))

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

(defun register (email password &key name phone bonuscard &allow-other-keys)
  "Create a new user with EMAIL and PASSWORD if one doesn't exist.
Otherwise throw ACCOUNT-ERROR"
  (when (eshop.odm:getobj 'user email)
    (error 'account-error :msg "Такой пользователь уже есть"))
  (make-instance 'user
                 :key email
                 :pass password
                 :name name
                 :phone phone
                 :bonuscard bonuscard))

(defun make-password-reset (email)
  "Creates a password-reset object and sends its data to user's email"
  (clean-tokens)
  (if-let ((user (eshop.odm:getobj 'user email)))
    (make-instance 'password-reset
                   :user user
                   :token (create-random-string 36 36))
    (error 'account-error :msg "На эту почту не зарегистрирован аккаунт")))

(defun find-password-reset (id token)
  (when-let ((reset (eshop.odm:getobj 'password-reset id)))
    (when (equal token (password-reset-token reset))
        reset)))

(defun apply-password-reset (reset new-password)
  "Resets a user's password using data created by MAKE-PASSWORD-RESET"
  (eshop.odm:with-transaction
    (let ((user (password-reset-user reset)))
      (setf (user-pass user) new-password)
      (eshop.odm:remobj reset))
    t))

(defun send-reset-email (reset &key (domain (hunchentoot:header-in* "HOST")))
  ;; TODO: send proper email
  (let* ((user (password-reset-user reset))
         (mail (user-email user))
         (body (format nil "http://~A/u/reset/~A?token=~A"
                       domain
                       (eshop.odm:serializable-object-key reset)
                       (password-reset-token reset))))
    (sendmail:send-email :to mail
                         :body body)))

(defun clean-tokens ()
  "Remove stale password reset tokens from the database"
  (eshop.odm:doobj (reset 'password-reset)
    (when (timeout-p (password-reset-timestamp reset)
                     (password-reset-timeout))
      (eshop.odm:remobj reset))))

(defun timeout-p (created timeout)
  (> (get-universal-time)
     (+ created timeout)))

;;;; Validation

(defun make-validation (user slot)
  (make-instance 'validation
                 :object user
                 :slot slot
                 :token (make-token slot)))

(defun make-token (slot)
  (case slot
    (email (create-random-string 36 36))
    (phone (create-random-string 8 10))))

(defun send-validation (validation &key (domain (hunchentoot:header-in* "HOST")))
  (case (validation-slot validation)
    ;; TODO: send proper email
    (email
     (let* ((user (validation-object validation))
            (mail (user-email user))
            (body (format nil "http://~A/u/validate?user=~A&token=~A"
                          domain
                          (eshop.odm:serializable-object-key validation)
                          (validation-token validation))))
       (sendmail:send-email :to mail
                            :body body)))
    (phone
     (cerror "Ignore" "Can't send SMS yet."))
    (t
     (cerror "Ignore" "Unknown slot to validate"))))

(defun validate-slot (id token)
  (if-let ((validation (eshop.odm:getobj 'validation id)))
    (if (equal token (validation-token validation))
      (eshop.odm:with-transaction
        (let ((object (validation-object validation)))
          (push (validation-slot validation)
                (slot-value object 'validations))
          (eshop.odm:remobj validation)
          object))
      (error 'account-error :msg "неправильный validation token"))
    (error 'account-error :msg "Неизвестный validation id")))

(declaim (ftype (function (user symbol) boolean) validated-p))
(defun validated-p (user slot)
  (not (null (member slot (slot-value user 'validations)))))

;;;; Require hunchentoot context

(defun current-user ()
  (session-user (start-session)))

(defun (setf current-user) (user)
  (eshop.odm:setobj (start-session)
                    'user user))
