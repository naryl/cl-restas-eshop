
(in-package eshop.proc)

(defvar *break-on-errors* nil)

(defclass process ()
  ((name :type string
         :initarg :name
         :reader process-name
         :initform "")
   (mutex :reader process-mutex
          :initform (bt:make-lock))
   (vars :accessor process-vars)
   (thread :accessor process-thread
           :initform nil)
   (mailbox :accessor process-mailbox
            :initform nil)))

(defvar *process* nil)

(defmethod print-object ((obj process) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (with-slots (name thread mailbox) obj
      (format stream "~A ~A {~A}"
              name
              (if thread "RUNNING" "STOPPED")
              (if mailbox
                  (mailbox-count mailbox)
                  "_")))))

(define-condition noproc (error)
  ((process :initarg :process))
  (:report (lambda (condition stream)
             (format stream "Process not running: ~S"
                     (slot-value condition 'process)))))

(defun running-p (process)
  (not (null (process-thread process))))

(defun ensure-running (process)
  (with-slots (name vars thread mailbox mutex) process
    (unless thread
      (bt:with-lock-held (mutex)
        (unless thread
          #+log4cl (log:info "Starting process ~A..." name)
          (let ((gate (make-gate :name "process init finished" :open nil)))
            (bt:make-thread #'(lambda ()
                                (start-process process gate))
                            :name name)
            (wait-on-gate gate)
            t))))))

(defun stop (process)
  (with-slots (mailbox thread) process
    (when thread
      (send-message mailbox :stop)
      (bt:join-thread thread)
      t)))

(defun kill (process)
  (with-slots (thread) process
    (bordeaux-threads:destroy-thread thread)))

(defun process-message (proc-name result gate func)
  (handler-case
      (progn
        (setf (aref result 0)
              (funcall func))
        (open-gate gate))
    (error (e)
      (setf (aref result 0)
            (list 'error e))
      (open-gate gate)
      #+log4cl (log:error "Error in process ~A: ~S" proc-name e)
      (when *break-on-errors* (invoke-debugger e)))))

(defun start-process (process finished-gate)
  (process-init process)
  (open-gate finished-gate)
  (unwind-protect
       (process-loop process)
    #+log4cl (log:warn "Process ~A exiting" (process-name process))
    (process-cleanup process)))

(defun process-loop (process)
  (with-slots (name thread mailbox) process
    (let ((*process* process))
      (loop (let ((message (receive-message mailbox)))
              (when (eq message :stop)
                (return))
              (restart-case
                  (apply #'process-message process message)
                (continue ()
                  nil)))))))

(defun process-init (process)
  (with-slots (mailbox vars thread) process
    (setf vars (make-hash-table))
    (setf mailbox (make-mailbox)
          thread (bt:current-thread))))

(defun process-cleanup (process)
  (with-slots (thread vars mailbox mutex) process
    (bt:with-lock-held (mutex)
      (setf thread nil)
      (setf vars nil)
      (let ((mailbox-tmp mailbox))
        (setf mailbox nil)
        (flush-mailbox process mailbox-tmp)))))

(defun flush-mailbox (process mailbox)
  (loop
     :until (mailbox-empty-p mailbox)
     :for message = (receive-message mailbox)
     :do (flush-message (make-instance 'noproc :process process) message)))

(defun flush-message (error message)
  (let+ (((result gate _) message))
    (setf (aref result 0)
          (list 'error error))
    (open-gate gate)))

(defun call (process func &key (reply :sync))
  (with-slots (mailbox thread) process
    (if thread
        (let ((result (make-array 1))
              (gate (make-gate :name "PROCESS-CALL GATE" :open nil)))
          (send-message mailbox (list result gate func))
          (if (eq reply :ignore)
              nil
              (flet ((get-reply ()
                       (wait-on-gate gate)
                       (let ((result (aref result 0)))
                         (if (and (listp result)
                                  (eq (first result) 'error))
                             (error (second result))
                             result))))
                (ecase reply
                  (:sync (get-reply))
                  (:async #'get-reply)))))
        (error 'noproc :process process))))

(defmacro exec ((process &optional (reply :sync)) (&rest vars) &body body)
  "Evaluates code in the process.
  REPLY is the type of result.
  :SYNC (default) waits for the return value and return it.
  :ASYNC returns a lambda which can be called to retrieve the result
  :IGNORE ignores the result *and any errors thrown from the code*"
  `(call ,process
         #'(lambda ()
             (proc-vars ,vars
               ,@body))
         :reply ,reply))

(defmacro proc-vars ((&rest vars) &body body)
  "Evaluate code using current process's variables.
  VARS is a list of symbols.
  Only valid inside PROCESS-EXEC or PROCESS-CALL"
  `(progn
     ,@(if vars
           (list (alexandria:with-gensyms (proc-vars)
                   `(let ((,proc-vars (process-vars *process*)))
                      (symbol-macrolet
                          ,(loop :for var :in vars
                              :collect `(,var (gethash ',var ,proc-vars)))
                        ,@body))))
           body)))
