
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
                                                             :message "Ошибка при вводе email")
                  :reader user-email)
   (pass :type (or null string)
         :serializable t
         :accessor user-pass
         :validation (%compile-nullable-validation-rule
                      (data-sift:compile-parse-rule 'string
                                                    :min-length 6 :max-length 100
                                                    :message "Пароль должен быть не короче шести символов"))
         :initform ""
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
   (bonuscard :type (or null bonuscard bonuscard-validation)
              :serializable t
              :initform nil
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

(defmethod initialize-instance ((user user) &rest args &key bonuscard)
  (remf args :bonuscard)
  (apply #'call-next-method user args)
  (when bonuscard
    (funcall +bonuscard-validator+ bonuscard)
    (make-bonuscard-validation user bonuscard)))

(defclass password-reset (eshop.odm:persistent-object)
  ((eshop.odm:key :reader password-reset-key)
   (user :serializable t
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
  ((eshop.odm:key :reader order-id)
   (phone :type string
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

(defmethod print-object ((obj order) stream)
  (when (slot-boundp obj 'order-state)
    (format stream "~A" (order-state obj))))

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
  ((eshop.odm:key :reader validation-key)
   (object :type eshop.odm:persistent-object
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

(defun register (email password name &key phone bonuscard &allow-other-keys)
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
    (let* ((reset (eshop.odm:regetobj reset))
           (user (password-reset-user reset)))
      (setf (user-pass user) new-password)
      (eshop.odm:remobj reset))
    t))

(defun send-reset-email (reset &key (domain (hunchentoot:header-in* "HOST")))
  ;; TODO: send proper email
  (let* ((user (password-reset-user reset))
         (mail (user-email user))
         (body (format nil "http://~A/u/reset/~A?token=~A"
                       domain
                       (password-reset-key reset)
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

;;;; Bonuscards

(defparameter +bonuscard-validator+
  (data-sift:compile-parse-rule 'string
                                :min-length 3 :max-length 30
                                :message "Ошибка при вводе номера бонусной карты"))

(defclass bonuscard (eshop.odm:persistent-object)
  ((eshop.odm:key :validation +bonuscard-validator+
                  :reader bonuscard-key)
   (count :type fixnum
          :initform 0
          :initarg :count
          :serializable t
          :accessor bonuscard-count))
  (:metaclass eshop.odm:persistent-class))

(defclass bonuscard-validation (eshop.odm:persistent-object)
  ((eshop.odm:key :reader bonuscard-validation-key)
   (user :type user
         :serializable t
         :reader bonuscard-validation-user
         :initarg :user)
   (bonuscard-key :type bonuscard-key
                  :serializable t
                  :reader bonuscard-validation-bonuscard-key
                  :reader bonuscard-key
                  :initarg :bonuscard-key)
   (token :type string
          :serializable t
          :initform (create-random-string 36 36)
          :reader bonuscard-validation-token)
   (rejected :type boolean
             :initform nil
             :serializable t
             :accessor bonuscard-validation-rejected))
  (:metaclass eshop.odm:persistent-class))

(defun make-bonuscard-validation (user bonuscard-key)
  "Создаёт запрос на валидацию бонусной карты и сохраняет его в пользователе.
  Удаляет предыдущий запрос, если он есть."
  (eshop.odm:with-transaction
    (let ((validation (make-instance 'bonuscard-validation
                                     :user user
                                     :bonuscard-key bonuscard-key))
          (user (eshop.odm:regetobj user)))
      (with-slots (bonuscard) user
        (when (typep bonuscard 'bonuscard-validation)
          (eshop.odm:remobj bonuscard))
        (setf bonuscard validation))
      validation)))

(defun make-bonuscard-validation-mail (validation &key (domain (hunchentoot:header-in* "HOST")))
  (with-slots (user bonuscard-key token (key eshop.odm:key)) validation
    (with-output-to-string (out)
      (format out "Запрос на валидацию бонусной карты~%")
      (format out "От пользователя: ~A <~A>~%"
              (user-name user)
              (user-email user))
      (format out "ЕКК: ~A~%" bonuscard-key)
      (when-let* ((bonuscard (eshop.odm:getobj 'bonuscard bonuscard-key))
                  (existing-user (first (eshop.odm:get-list 'user :query (son 'bonuscard bonuscard)))))
        (format out "Эта карта уже зарегистрирована на пользователя ~A <~A>~%"
                (user-name existing-user)
                (user-email existing-user)))
      (format out "Подтвердить: http://~A/u/confirm-bonuscard?id=~A&token=~A~%"
              domain key token)
      (format out "Отклонить: http://~A/u/reject-bonuscard?id=~A&token=~A~%"
              domain key token))))

(defun send-bonuscard-validation-mail (validation)
  "Отправляет письмо с запросом подтверждения по указанным в конфиге картам"
  (let ((mails (config.get-option :critical :ekk-emails))
        (body (make-bonuscard-validation-mail validation)))
    (sendmail:send-email :to mails
                         :body body)))

(defun user-bonus (user)
  "Возвращает количество бонусов на бонусной карте пользователя, если она
подтверждена. Иначе - nil"
  (awhen (slot-value user 'bonuscard)
    (when (typep it 'bonuscard)
      (bonuscard-count it))))

(defun user-bonuscard-valid-p (user)
  "Возвращает :valid :pending или :invalid в зависимости от состояния валидации бонусной карты"
  (when-let ((bonuscard (slot-value user 'bonuscard)))
    (etypecase bonuscard
      (bonuscard :valid)
      (bonuscard-validation (if (bonuscard-validation-rejected bonuscard)
                                :invalid
                                :pending)))))

(defun confirm-bonuscard (id remote-token)
  "Принимает запрос валидации бонусной карты и сохраняет ссылку в пользователе"
  (eshop.odm:with-transaction
    (let ((validation (eshop.odm:getobj 'bonuscard-validation id)))
      (with-slots (user bonuscard-key token)
          validation
        (when (equal token remote-token)
          (setf (slot-value user 'bonuscard)
                (or (eshop.odm:getobj 'bonuscard bonuscard-key)
                    (make-instance 'bonuscard :key bonuscard-key)))
          (eshop.odm:remobj validation)
          t)))))

(defun reject-bonuscard (id remote-token)
  "Отмечает попытку валидации бонусной карты отклонённой"
  (eshop.odm:with-transaction
    (let ((validation (eshop.odm:getobj 'bonuscard-validation id)))
      (with-slots (token)
          validation
        (when (equal token remote-token)
          (eshop.odm:setobj validation 'rejected t)
          t)))))

;;;; Validation

(defun make-validation (user slot)
  (make-instance 'validation
                 :object user
                 :slot slot
                 :token (make-token slot)))

(defun make-token (slot)
  (ecase slot
    (email (create-random-string 36 36))
    (phone (create-random-string 8 10))))

(defun send-validation (validation &key (domain (hunchentoot:header-in* "HOST")))
  (let ((user (validation-object validation)))
    (ecase (validation-slot validation)
      ;; TODO: send proper email
      (email
       (let ((mail (user-email user))
             (body (format nil "http://~A/u/validate?id=~A&token=~A"
                           domain
                           (validation-key validation)
                           (validation-token validation))))
         (sendmail:send-email :to mail
                              :body body)))
      (phone
       (cerror "Ignore" "Can't send SMS yet."))
      (t
       (cerror "Ignore" "Unknown slot to validate")))))

(defun validate-slot (id token)
  (if-let ((validation (eshop.odm:getobj 'validation id)))
    (if (equal token (validation-token validation))
      (eshop.odm:with-transaction
        (let ((object (validation-object validation)))
          (push (validation-slot validation)
                (slot-value object 'validations))
          (eshop.odm:remobj validation)
          object))
      (error 'account-error :msg "Неправильный validation token"))
    (error 'account-error :msg "Неизвестный validation id")))

(defun reset-validation (user slot)
  "Make slot non-validated"
  (eshop.odm:with-transaction
    (deletef (slot-value (eshop.odm:regetobj user) 'validations) slot)))

(declaim (ftype (function (user symbol) boolean) validated-p))
(defun validated-p (user slot)
  (not (null (member slot (slot-value user 'validations)))))

;;;; Require hunchentoot context

(defun current-user ()
  (awhen (start-session)
    (session-user it)))

(defun (setf current-user) (user)
  (awhen (start-session)
      (eshop.odm:setobj it 'user user)))
