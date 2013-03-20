(in-package #:eshop)

(arnesi:enable-sharp-l-syntax)

(alexandria:define-constant +phone-next-template+
    (make-instance 'sendmail:email
                   :from (config.get-option :critical :from-email)
                   :type "text" :subtype "html")
  :test (constantly t)
  :documentation "Template for email to client about his/her order")

(defun email.phone-next-mail (body)
  (declare (string body))
  (sendmail:send-email-with-template
   +phone-next-template+
   :to (config.get-option :critical :order-emails)
   :subject (format nil "www.320-8080.ru - ЗАКАЗ ЗВОНКА")
   :body body))

(defclass phone-next.answer ()
  ((phone     :initarg :phone     :initform nil   :accessor phone)
   (errorid   :initarg :errorid   :initform nil   :accessor errorid)))

(define-tracing-route phone-next-route ("/phone_next")
  (phone-next (request-get-plist)))

(defun phone-next (request-get-plist)
  (let* ((phone (getf request-get-plist :phone))
         (error-id 0)
         (answer (make-instance 'phone-next.answer :phone phone)))
    (if phone
        (email.phone-next-mail (soy.sendmail:phonemail (list :phone phone)))
        (setf error-id 2)) ;; no phone number in parameters
    (setf (errorid answer) error-id)
    (json:encode-json-to-string answer)))
