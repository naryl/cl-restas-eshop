;;;; bonus-activate.lisp

(in-package #:eshop)

(defvar *bonus-ekk* (make-hash-table :test #'equal))

(restas:define-route activatebonus-route ("/activatebonus2"
                                          :decorators '(@timer))
  (bonus-activate.proccess))

(defun bonus-activate.proccess ()
  (let ((telef (hunchentoot:parameter "phone"))
        (name (hunchentoot:parameter "name"))
        (email (hunchentoot:parameter "email"))
        (ekk (hunchentoot:parameter "ekk")))
    (if (and
         ekk
         (= 6 (length ekk))
         (string<= "070000" ekk)
         (string>= "085024" ekk)
         (not (gethash ekk *bonus-ekk*)))
        (progn
          (setf (gethash ekk *bonus-ekk*) t)
          (bonus-activate.email telef name email ekk)
          (hunchentoot:redirect "/" :code 301))
        (progn
          (hunchentoot:redirect "/" :code 301)))))

(alexandria:define-constant +bonus-mail-template+
    (make-instance 'sendmail:email
                   :from (config.get-option :critical :from-email)
                   :type "text" :subtype "html"
                   :to (config.get-option :critical :order-emails))
  :test (constantly t))


(defun bonus-activate.email (telef name email ekk)
  (sendmail:send-email-with-template
   +bonus-mail-template+
   :subject (format nil "bonus ~D" ekk)
   :body (format nil "Телефон:~A<br/>~&ФИО:~A<br/>~&email:~A<br/>~&EKK:~A<br/>~&" telef name email ekk)))
