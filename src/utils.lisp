
(in-package eshop.utils)

;;;; Timers

;; SBCL's timers stop working if one timer fails to return in time.
;; This macro makes it hard to forget to use a timeout :)
(defmacro deftimer ((name interval &optional (timeout 0)) &body body)
  "Defines a function with timeout (you can call it manually) and registers a timer to
execute it each INTERVAL seconds. Use START-TIMER and STOP-TIMER to start and stop it."
  (check-type name symbol)
  `(progn
     (defun ,name ()
       (handler-case
           (sb-ext:with-timeout ,timeout
             ,@body)
         (sb-ext:timeout ()
           (log:error "Timeout in timer ~A" ',name))))
     (defparameter ,name (cons (sb-ext:make-timer #',name)
                               ,interval))))

(defun start-timer (timer)
  (sb-ext:schedule-timer (car timer) (cdr timer)
                         :repeat-interval (cdr timer)))

(defun stop-timer (timer)
  (sb-ext:unschedule-timer (car timer)))

;;;; Time

(defvar *unix-epoch-difference*
  (encode-universal-time 0 0 0 1 1 1970 0))

(defun unix-to-universal-time (unix-time)
  (+ unix-time *unix-epoch-difference*))

(defun universal-to-unix-time (universal-time)
  (- universal-time *unix-epoch-difference*))

(defun get-unix-time (&optional (universal-time (get-universal-time)))
  (universal-to-unix-time universal-time))

(define-constant
    +date+ '(:day "." :month "." :year)
  :test #'equal)

(defun render-time (format &optional (time (get-universal-time)))
  (let ((ts (universal-to-timestamp time)))
    (format-timestring nil ts :format
                       format)))

(defun optional-parameter (name)
  (let ((string (hunchentoot:parameter name)))
    (when (string/= string "")
      string)))

(defmacro with-hunchentoot-parameters ((&rest parameters) &body body)
  "Fetches hunchentoot parameters. Prepend symbol with @ to replace it with NIL if it's empty."
  `(let ,(loop
            :for param :in parameters
            :collect (let ((raw-name (string-downcase (string param))))
                       `(,param
                         ,(if (char= #\@ (elt raw-name 0))
                              `(optional-parameter ,(subseq raw-name 1))
                              `(hunchentoot:parameter ,raw-name)))))
     ,@body))

;;;; AJAX

(defmacro define-ajax-route (name (template) &body body)
  `(restas:define-route ,name (,template :method :post)
     ,@(remove-if-not #'(lambda (item) (keywordp (car item))) body)
     (st-json:write-json-to-string
      (progn
        ,@(remove-if #'(lambda (item) (keywordp (car item))) body)))))

;;;; JSON

(defun write-object-to-json (object)
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (slot (c2cl:class-direct-slots (class-of object)))
      (let ((slot-name (slot-definition-name slot)))
        (setf (gethash (string-downcase (string slot-name)) ht)
              (slot-value object slot-name))))
    (st-json:write-json-to-string ht)))
