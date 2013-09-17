
(in-package eshop.proc)

(defclass process ()
  ((name :type string
         :initarg :name
         :reader process-name
         :initform "")
   (mutex :reader process-mutex
          :initform (bt:make-lock))
   (thread :accessor process-thread
           :initform nil)
   (mailbox :accessor process-mailbox
            :initform nil)))

(defmethod print-object ((obj process) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (with-slots (name thread mailbox) obj
      (format stream "~A ~A {~A}"
              name
              (if thread "RUNNING" "STOPPED")
              (if mailbox
                  (mailbox-count mailbox)
                  "_")))))

(define-condition noproc ()
  ((process :initarg :process)))

(defun process-running (process)
  (not (null (process-thread process))))

(defun ensure-process (process)
  (with-slots (name thread mailbox mutex) process
    (unless thread
      (bt:with-lock-held (mutex)
        (unless thread
          (log:info "Starting process ~A..." name)
          (let ((gate (make-gate :name "process init finished" :open nil)))
            (bt:make-thread #'(lambda ()
                                (start-process process gate))
                            :name name)
            (wait-on-gate gate)
            t))))))

(defun stop-process (process)
  (with-slots (mailbox thread) process
    (when thread
      (send-message mailbox :stop)
      (bt:join-thread thread)
      t)))

(defun process-message (proc-name result gate func)
  (handler-case
      (progn
        (setf (aref result 0)
              (funcall func))
        (open-gate gate))
    (error (e)
      (setf (aref result 0) (list 'error e))
      (open-gate gate)
      #-debug (log:error "Error in process ~A: ~S" proc-name e)
      #+debug (invoke-debugger e))))

(defun start-process (process finished-gate)
  (process-init process)
  (open-gate finished-gate)
  (unwind-protect
       (process-loop process)
    (log:warn "Process ~A exiting" (process-name process))
    (process-cleanup process)))

(defun process-loop (process)
  (with-slots (name thread mailbox) process
    (loop (awhen (receive-message mailbox)
            (when (eq it :stop)
              (return))
            (restart-case
                (apply #'process-message process it)
              (continue ()
                :report "Report error to caller and continue"
                (flush-message (make-instance 'noproc :process process)
                               it)))))))

(defun process-init (process)
  (with-slots (mailbox thread) process
    (setf mailbox (make-mailbox)
          thread (bt:current-thread))))

(defun process-cleanup (process)
  (with-slots (thread mailbox mutex) process
    (bt:with-lock-held (mutex)
      (setf thread nil)
      (let ((mailbox-tmp mailbox))
        (setf mailbox nil)
        (flush-mailbox process mailbox-tmp)))))

(defun flush-mailbox (process mailbox)
  (loop
     :until (mailbox-empty-p mailbox)
     :for message = (receive-message mailbox)
     :do (flush-message (make-instance 'nopoc :process process) message)))

(defun flush-message (error message)
  (destructuring-bind (result gate proc)
      message
    (declare (ignore proc))
    (setf (aref result 0) (list 'error error))
    (open-gate gate)))

(defun process-call (process func)
  (with-slots (mailbox thread) process
    (if thread
        (let ((result (make-array 1))
              (gate (make-gate :name "PROCESS-CALL GATE" :open nil)))
          (send-message mailbox (list result gate func))
          (wait-on-gate gate)
          (let ((result (aref result 0)))
            (if (and (listp result)
                     (eq (first result) 'error))
                (error (second result))
                result)))
        (error 'noproc))))

(defmacro process-exec ((process) &body body)
  `(process-call ,process #'(lambda ()
                             ,@body)))
