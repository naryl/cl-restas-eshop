
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
