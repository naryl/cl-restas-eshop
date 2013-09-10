
;;;; mop-odm.lisp

;;;;
;;
;; This file implements simple object-document mapping (ODM) and serialization.
;;
;; To make a class serializable make it an instance of SERIALIZABLE-CLASS
;; and mark a few slots with :serializable t. Then use SERIALIZE and DESERIALIZE to
;; turn it into different formats e.g. :hashtable or :json
;;
;; To make a class persistent make it an instance of PERSISTENT-CLASS and mark slots
;; with :serializable t. MAKE-INSTANCE will automatically store the object in mongo.
;;
;; GETOBJ class key
;; Fetches the object from mongo. If the object contains references they will become lazy
;; references and only fetch the referenced object once it's accessed. Objects are
;; read-only when outside transaction.
;;
;; REMOBJ class key
;; REMOBJ object
;; Removes object from database. The second form makes it read-only
;;
;; WITH-TRANSACTION body...
;; When inside this macro you can get objects and set their slots. They'll be written
;; back into mongo as soon as the transaction closes. Be aware that objects become read-only
;; if the transaction was committed or invalid if it was rolled back. Commit is
;; automatic at end of body. When several transactions are nested all but the outermost
;; are ignored.
;;
;; ROLLBACK-TRANSACTION
;; Aborts the current transaction. The DB is untouched and the objects become invalid.
;; This is done automatically when transaction terminates with condition.
;;
;;;;

(in-package eshop.odm)

(declaim (optimize (safety 3) (debug 3)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro db-eval (&body body)
    `(db-call #'(lambda ()
                  ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SERIALIZABLE

;;;; Metaclasses

(defclass serializable-class (standard-class)
  ()
  (:documentation "Metaclass for serializable objects"))

(defmethod validate-superclass ((sub serializable-class) (super standard-class))
  t)

(defclass serializable-direct-slot-definition (standard-direct-slot-definition)
  ((serializable :initarg :serializable
                 :type boolean
                 :initform nil
                 :reader slot-serializable-p)))

(defclass serializable-effective-slot-definition (standard-effective-slot-definition)
  ((serializable :initarg :serializable
                 :type boolean
                 :initform nil
                 :reader slot-serializable-p)))

(defmethod direct-slot-definition-class ((class serializable-class) &rest initargs)
  (declare (ignore initargs))
  'serializable-direct-slot-definition)

(defmethod effective-slot-definition-class ((class serializable-class) &rest initargs)
  (declare (ignore initargs))
  'serializable-effective-slot-definition)

(defmethod slot-serializable-p ((slot standard-direct-slot-definition))
  "Slots are not serializable unless they're SERIALIZABLE-SLOTs"
  nil)

(defmethod compute-effective-slot-definition :around ((class serializable-class) slot-name direct-slot-definitions)
  (let ((slot (call-next-method)))
    (setf (slot-value slot 'serializable)
          (some #'slot-serializable-p direct-slot-definitions))
    slot))

(defclass serializable-object ()
  ((key :initarg :key
        :serializable t
        :initform (unless *deserializing* (next-autokey))
        :reader serializable-object-key))
  (:documentation "Root of serializable objects.")
  (:metaclass serializable-class))

;;;; Serialization

(defvar *deserializing* nil)

(define-constant +class-key+ "%CLASS%" :test #'equal)

(defun serialize (obj &optional (target :hashtable))
  (serialize% obj target))

(defgeneric serialize% (obj target)
  (:documentation "Serializes OBJ to TARGET data structure.
  SERIALIZE and DESERIALIZE are inverse. If you successfully serialized an object
  deserializing it should always yield exact same object.")
  (:method ((obj serializable-object) (target (eql :json)))
    (st-json:write-json-to-string (serialize obj :hashtable)))
  (:method ((obj serializable-object) (target (eql :hashtable)))
    (let* ((class (class-of obj))
           (slots (class-slots class))
           (ht (make-hash-table :test #'equal)))
      (setf (gethash +class-key+ ht)
            (symbol-fqn (class-name class)))
      (dolist (slot slots)
        (when (slot-serializable-p slot)
          (let ((slot-name (slot-definition-name slot)))
            (when (slot-boundp obj slot-name)
              (setf (gethash (symbol-fqn slot-name) ht)
                    (serialize (slot-value obj slot-name) :hashtable))))))
      ht))
  (:method ((string string) (target (eql :hashtable)))
    string)
  (:method ((number number) (target (eql :hashtable)))
    number))

(defun deserialize (data &optional (source :hashtable))
  (deserialize% data source))

(defgeneric deserialize% (data source)
  (:documentation "Deserializes object from DATA using SOURCE data structure.
  SERIALIZE and DESERIALIZE are inverse. If you successfully serialized an object
  deserializing it should always yield exact same object.")
  (:method ((string string) (target (eql :json)))
    (let ((st-json:*decode-objects-as* :hashtable)
          (st-json:*map-keys* #'identity))
      (deserialize (st-json:read-json-from-string string) :hashtable)))
  (:method ((ht hash-table) (source (eql :hashtable)))
    (let ((*deserializing* t))
      (let* ((class-name (read-from-string (gethash +class-key+ ht)))
             (class (find-class class-name)))
        (make-instance class 'ht ht))))
  (:method ((string string) (source (eql :hashtable)))
    string)
  (:method ((number number) (source (eql :hashtable)))
    number))

(defmethod shared-initialize :around ((instance serializable-object) slots &rest initargs &key ((ht ht)) &allow-other-keys)
  (if *deserializing*
      (progn
        (apply #'call-next-method instance nil initargs)
        (maphash #'(lambda (k v)
                     (unless (string= k +class-key+)
                       (let ((slot-name (fqn-symbol k)))
                         (setf (slot-value instance slot-name)
                               (deserialize v)))))
                 ht)
        (setf (slot-value instance 'modified) nil))
      (call-next-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PERSISTENT

;;;; Metaclasses

(defclass persistent-class (serializable-class)
  ((versioned :initform nil
              :type boolean
              :reader persistent-class-versioned)))

(defmethod persistent-class-versioned ((class standard-class))
  (declare (ignore class))
  nil)

(defmethod initialize-instance :after ((class persistent-class)
                                        &key direct-slots direct-superclasses
                                        &allow-other-keys)
  (setf (slot-value class 'versioned)
        (or (some #'(lambda (slot) (getf slot :versioned)) direct-slots)
            (some #'persistent-class-versioned direct-superclasses))))

(defmethod validate-superclass ((sub persistent-class) (super standard-class))
  t)

(defmethod validate-superclass ((sub persistent-class) (super serializable-class))
  t)

(defclass persistent-direct-slot-definition (serializable-direct-slot-definition)
  ((versioned :initform nil
              :type boolean
              :initarg :versioned
              :reader slot-versioned)))

(defclass persistent-effective-slot-definition (serializable-effective-slot-definition)
  ((versioned :initform nil
              :type boolean
              :initarg :versioned
              :reader slot-versioned)
   (modified :initform nil
             :type boolean
             :accessor slot-modified)))

(defmethod slot-versioned ((slot serializable-direct-slot-definition))
  (declare (ignore slot))
  nil)

(defmethod slot-versioned ((slot standard-direct-slot-definition))
  (declare (ignore slot))
  nil)

(defmethod direct-slot-definition-class ((class persistent-class) &rest initargs)
  (declare (ignore initargs))
  'persistent-direct-slot-definition)

(defmethod effective-slot-definition-class ((class persistent-class) &rest initargs)
  (declare (ignore initargs))
  'persistent-effective-slot-definition)

(defmethod compute-effective-slot-definition :around
    ((class persistent-class) slot-name direct-slot-definitions)
  (let ((slot (call-next-method))
        (versioned (some #'slot-versioned direct-slot-definitions)))
    (when (and versioned
               (not (slot-serializable-p slot)))
      (error "Versioned but not serializable slot makes no sense. Slot: ~A. Class: ~A"
             slot-name class))
    (setf (slot-value slot 'versioned) versioned)
    slot))

(defclass persistent-object (serializable-object)
  ((state :initform :rw
          :type (member :rw :ro :deleted)
          :accessor persistent-object-state)
   (modified :initform nil
             :type boolean
             :accessor persistent-object-modified))
  (:documentation "Root of persistent objects.")
  (:metaclass persistent-class))

(defmethod print-object ((obj persistent-object) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (if (and (slot-boundp obj 'key)
             (slot-boundp obj 'state))
        (case (persistent-object-state obj)
          ((:rw :ro) (format stream "~A ~A"
                             (persistent-object-state obj)
                             (serializable-object-key obj)))
          ((:deleted) (format stream "DELETED")))
        (format stream "DUMMY"))))

;;;; Persistence

(defvar *server-config*)
(defvar *db-name*)
(defvar *mongo-client*)
(defvar *db*)

(defvar *db-cache-timer* (sb-ext:make-timer #'purge-all-caches
                    :name "Cache cleaner"
                    :thread t))

(defun connect (database &rest server-config &key hostname port)
  "Sets the server parameters and creates a mongo connection. Reconnection is automatic in
  case of network failure."
  (declare (ignore hostname port))
  (setf *server-config* (apply #'make-instance 'mongo:server-config server-config))
  (setf *db-name* database)
  (unless (sb-ext:timer-scheduled-p *db-cache-timer*)
    (sb-ext:schedule-timer *db-cache-timer*
                           60 :repeat-interval 60))
  (reconnect)
  (values))

(defun reconnect ()
  (log:info "Connecting to mongo...")
  (unless (and *db-name* *server-config*)
    (error "Can't reconnect before making initial connection using CONNECT"))
  (when *db-thread*
    (send-message *db-mailbox* :stop)
    (bt:join-thread *db-thread*))
  (ignore-errors
    (mongo-cl-driver.adapters:mongo-client-close *mongo-client*))
  (setf *mongo-client* (make-instance 'mongo.usocket:mongo-client :server *server-config*))
  (setf *db* (make-instance 'mongo:database
                            :name *db-name*
                            :mongo-client *mongo-client*))
  (ensure-db-thread))

(defmethod initialize-instance :before ((obj persistent-object) &key &allow-other-keys)
  (setf (slot-value obj 'state) :rw))

(defmethod initialize-instance :after ((obj persistent-object) &key &allow-other-keys)
  "Stores newly created persistent object in the database"
  (unless *deserializing*
    (if *transaction*
        (setf (slot-value obj 'modified) t)
        (store-instance obj))
    (new-transaction-object obj)))

(defun store-instance (obj)
  (let ((collection (obj-collection obj))
        (ht (serialize obj)))
      (db-eval
        (mongo:insert-op collection ht))))

(defun delete-instance (obj)
  (db-eval
    (mongo:delete-op (obj-collection obj)
                     (son (symbol-fqn 'key) (serializable-object-key obj)))))

(defun update-instance (obj)
  (let ((class (class-of obj)))
    (let ((ht (serialize obj))
          (collection (obj-collection obj))
          (version-collection (obj-collection obj t))
          (slot-changes (and (persistent-class-versioned class)
                             (collect-slot-changes obj))))
      (db-eval
        (mongo:delete-op collection (son (symbol-fqn 'key) (serializable-object-key obj)))
        (mongo:insert-op collection ht)
        (when slot-changes
          (mongo:insert-op version-collection slot-changes))))))

(defun collect-slot-changes (obj)
  (let ((time (get-unix-time)))
    (mapcan #'(lambda (slot)
                (when (and (slot-versioned slot)
                           (slot-modified slot))
                  (setf (slot-modified slot) nil)
                  (let ((slot-name (slot-definition-name slot)))
                    (list
                     (son +class-key+ (symbol-fqn (type-of obj))
                          (symbol-fqn 'key) (serializable-object-key obj)
                          "TIMESTAMP" time
                          "SLOT" (symbol-fqn slot-name)
                          "VALUE" (serialize (slot-value obj slot-name)))))))
            (class-slots (class-of obj)))))

(defvar *transaction* nil)

(defmacro with-transaction (&body body)
  "All modified objects will be sent back to mongo when the transaction exits normally"
  (with-gensyms (transaction)
    `(flet ((,transaction ()
              ,@body))
       (if *transaction*
           (,transaction)
           (catch 'rollback-transaction
             (let ((*transaction* (list nil)))
               (values
                (prog1
                    (,transaction)
                  (commit-transaction))
                t)))))))

(defmethod slot-value-using-class :around
    ((class persistent-class) obj (slot persistent-effective-slot-definition))
  (if (or (member (slot-definition-name slot)
                  '(state key modified))
          (not (eq :deleted (persistent-object-state obj))))
      (call-next-method)
      (error "Attempt to access a slot of deleted persistent object")))

(defmethod (setf slot-value-using-class) :around
    (new-value (class persistent-class) obj (slot persistent-effective-slot-definition))
  (cond ((and (not *deserializing*)
              (slot-boundp obj 'state)
              (not (eq :rw (persistent-object-state obj))))
         (error "Attempt to modify a slot of read-only persistent object"))
        ((and (slot-serializable-p slot)
              (not *deserializing*)
              (not (equal new-value (slot-value obj (slot-definition-name slot)))))
         (setf (persistent-object-modified obj) t
               (slot-modified slot) t)))
  (call-next-method))


(defun commit-transaction ()
  (unless *transaction*
    (error "Attempt to commit while not inside transaction"))
  (dolist (obj *transaction*)
    (when (and obj
               (persistent-object-modified obj))
      (clear-cache-partial-arguments *getobj-cache-cache* (list (type-of obj) (serializable-object-key obj)))
      (case (persistent-object-state obj)
        (:rw
         (setf (persistent-object-state obj) :ro)
         (update-instance obj))
        (:deleted
         (delete-instance obj))))))

(defun rollback-transaction ()
  "Invalidates all transaction objects. Doesn't modify the DB."
  (cond (*transaction*
         (dolist (obj *transaction*)
           (when obj
             (setf (persistent-object-state obj)
                   :deleted)))
         (throw 'rollback-transaction (values nil nil)))
        (t
         (error "Attempt to rollback while not inside transaction"))))

(defun new-transaction-object (obj)
  (setf (persistent-object-state obj)
        (cond (*transaction*
               (pushnew obj *transaction*
                        :key (lambda (o) (when o (serializable-object-key o)))
                        :test #'equal)
               :rw)
              (t
               :ro)))
  obj)

(defcached (getobj-cache :timeout 600) (class key)
  (let ((ht (let ((collection (obj-collection class)))
                (db-eval
                  (mongo:find-one collection
                                  :query (son (symbol-fqn 'key) key)
                                  :selector (son "_id" 0))))))
      (when (and ht
                 (string= (symbol-fqn class)
                          (gethash +class-key+ ht)))
        ht)))

(defun getobj (class key)
  "Fetch an object from the database. The object will be read-only unless it's done inside
  a transaction."
  (metric:count "getobj")
  (if *transaction*
      (awhen (find key *transaction* :key #'(lambda (o)
                                              (when o
                                                (serializable-object-key o))))
        (return-from getobj it)))
  (when-let* ((db-obj (getobj-cache class key))
              (obj (deserialize db-obj)))
    (new-transaction-object obj)))

(defun remobj (obj)
  "Deletes an object from the database"
  (setf (persistent-object-state obj) :deleted)
  (setf (persistent-object-modified obj) t)
  (unless *transaction*
    (delete-instance obj)))

(defun setobj (obj-or-class &rest slots-values)
  "Modifies an object in a transaction.
  Call either as (setobj (getobj 'class 1234) 'a 1 'b 2)
  or (setobj 'class 1234 'a 1 'b 2)"
  (flet ((invalid-arguments ()
           (error "SETOBJ requires either persistent-object or both class and key")))
    (let (class key)
      (typecase obj-or-class
        (symbol
         (setf class obj-or-class
               key (pop slots-values)))
        (persistent-object
         (setf class (type-of obj-or-class)
               key (serializable-object-key obj-or-class)))
        (t
         (invalid-arguments)))
      (with-transaction
        (let ((obj (getobj class key)))
          (doplist (slot value slots-values)
            (setf (slot-value obj slot) value))
          obj)))))

(defun all-keys (class)
  (db-eval
    (mapcar #'(lambda (obj)
                (gethash (symbol-fqn 'key) obj))
            (mongo:find-list (obj-collection class)
                             :query (son +class-key+ (symbol-fqn class))
                             :fields (son (symbol-fqn 'key) 1
                                          "_id" 0)))))

(defun mapobj (function class)
  (when *transaction*
    (error "MAPOBJ in a transaction is a Bad Thing. Do a transaction for each
  iteration if you really need it."))
  (nreverse
   (let (result)
     (dolist (key (all-keys class) result)
       (push (funcall function (getobj class key)) result))
     result)))

(defmacro doobj ((obj-var class &optional result-raw) &body body)
  (let* ((result-raw (ensure-list result-raw))
         (result-name (first result-raw))
         (result-init (second result-raw)))
    `(let (,@(cond ((null result-name)
                    nil)
                   ((symbolp result-name)
                    `((,result-name ,result-init)))))
       (when *transaction*
         (error "DOOBJ in a transaction is a Bad Thing. Do a transaction for each
  iteration if you really need it."))
       (dolist (,obj-var (all-keys ,class))
         (let ((,obj-var (getobj ,class ,obj-var)))
           ,@body))
       ,result-name)))

;;;; DB thread

(defvar *db-thread* nil)

(defvar *db-mailbox* (make-mailbox :name "DB-MAILBOX"))

(defun ensure-db-thread ()
  (unless *db-thread*
    (log:info "Starting DB thread...")
    (bt:make-thread #'db-thread :name "DB-THREAD")))

(defun process-message (result gate func)
  (handler-case
      (progn
        (setf (aref result 0)
              (funcall func))
        (open-gate gate))
    (error (e)
      (setf (aref result 0) (list 'error e))
      (open-gate gate)
      #-debug (log:error "Error in DB thread: ~S" e)
      #+debug (invoke-debugger e))))

(defun db-thread ()
  (setf *db-thread* (bt:current-thread))
  (unwind-protect
       (loop (awhen (receive-message *db-mailbox* :timeout 1)
               (when (eq it :stop)
                 (return))
               (apply #'process-message it)))
    (log:warn "Database thread exiting")
    (setf *db-thread* nil)))

(defun db-call (func)
  (ensure-db-thread)
  (let ((result (make-array 1))
        (gate (make-gate :name "DB-CALL GATE" :open nil)))
    (send-message *db-mailbox* (list result gate func))
    (wait-on-gate gate)
    (let ((result (aref result 0)))
      (if (and (listp result)
               (eq (first result) 'error))
          (error (second result))
          result))))

;;;; Utils

(defcached symbol-fqn (symbol)
  (substitute #\; #\.
              (concatenate 'string
                           (package-name (symbol-package symbol))
                           "::"
                           (symbol-name symbol))))

(defcached fqn-symbol (fqn)
  (read-from-string (substitute #\. #\; fqn)))

(let ((counter (make-array 1 :initial-element 1 :element-type 'sb-ext:word)))
  (defun next-autokey ()
    (let ((new-counter (sb-ext:atomic-incf (aref counter 0))))
      (+ new-counter
         (* (get-universal-time)
            (expt 10 (1+ (truncate (log new-counter 10)))))))))

(defgeneric obj-collection (obj &optional versions)
  (:method ((class symbol) &optional versions)
    (declare (ignore versions))
    (string class))
  (:method ((obj persistent-object) &optional versions)
    (declare (ignore versions))
    (string (type-of obj)))
  (:method :around (obj &optional versions)
    (mongo:collection *db* (format nil "~A~:[~;_VERSIONS~]"
                                   (call-next-method)
                                   versions))))

(metric:defmetric getobj-cache-size ("getobj.cache")
  (cached-results-count *getobj-cache-cache*))

(defvar *unix-epoch-difference*
  (encode-universal-time 0 0 0 1 1 1970 0))

(defun universal-to-unix-time (universal-time)
  (- universal-time *unix-epoch-difference*))

(defun get-unix-time (&optional (universal-time (get-universal-time)))
  (universal-to-unix-time universal-time))
