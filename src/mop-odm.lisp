
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
;; WITH-TRANSACTION body...
;; When inside this macro you can get objects and set their slots. They'll be written
;; back into mongo as soon as the transaction closes. Be aware that objects become read-only
;; if the transaction was committed or invalid if it was rolled back. Commit is
;; automatic at end of body.
;;
;; ROLLBACK-TRANSACTION
;; Aborts the current transaction. The DB is untouched and the objects become invalid.
;; This is done automatically when transaction terminates with condition.
;;
;;;;

(in-package eshop.odm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SERIALIZABLE

;;;; Metaclasses

(defclass serializable-class (standard-class)
  ()
  (:documentation "Metaclass for serializable objects"))

(defmethod validate-superclass ((sub serializable-class) (super standard-class))
  t)

(defclass serializable-direct-slot-definition (standard-direct-slot-definition)
  ((serializable :initarg :serializable
                 :initform nil
                 :reader slot-serializable-p)))

(defclass serializable-effective-slot-definition (standard-effective-slot-definition)
  ((serializable :initarg :serializable
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
        :initform (next-autokey)
        :reader serializable-object-key))
  (:documentation "Root of serializable objects.")
  (:metaclass serializable-class))

;;;; Serialization

(defvar *default-target* :hashtable)

(alexandria:define-constant +class-key+ "%CLASS%" :test #'equal)

(defgeneric serialize (obj target)
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
            (string (class-name class)))
      (dolist (slot slots)
        (when (slot-serializable-p slot)
          (let ((slot-name (slot-definition-name slot)))
            (setf (gethash (string slot-name) ht)
                  (serialize (slot-value obj slot-name) :hashtable)))))
      ht))
  (:method ((string string) (target (eql :hashtable)))
    string)
  (:method ((number number) (target (eql :hashtable)))
    number))

(defgeneric deserialize (data source)
  (:documentation "Deserializes object from DATA using SOURCE data structure.
  SERIALIZE and DESERIALIZE are inverse. If you successfully serialized an object
  deserializing it should always yield exact same object.")
  (:method ((string string) (target (eql :json)))
    (let ((st-json:*decode-objects-as* :hashtable)
          (st-json:*map-keys* #'identity))
      (deserialize (st-json:read-json-from-string string) :hashtable)))
  (:method ((ht hash-table) (source (eql :hashtable)))
    (let* ((class-name (read-from-string (gethash +class-key+ ht)))
           (class (find-class class-name))
           (obj (make-instance class)))
      (maphash #'(lambda (k v)
                   (unless (string= k +class-key+)
                     (let ((slot-name (read-from-string k)))
                       (setf (slot-value obj slot-name)
                             (deserialize v :hashtable)))))
               ht)
      obj))
  (:method ((string string) (source (eql :hashtable)))
    string)
  (:method ((number number) (source (eql :hashtable)))
    number))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PERSISTENT

;;;; Metaclasses

(defclass persistent-class (standard-class) ())

(defmethod validate-superclass ((sub persistent-class) (super standard-class))
  t)

(defmethod validate-superclass ((sub persistent-class) (super serializable-class))
  t)

(defclass persistent-direct-slot-definition (serializable-direct-slot-definition)
  ())

(defclass persistent-effective-slot-definition (serializable-effective-slot-definition)
  ())

(defmethod direct-slot-definition-class ((class persistent-class) &rest initargs)
  (declare (ignore initargs))
  'persistent-direct-slot-definition)

(defmethod effective-slot-definition-class ((class persistent-class) &rest initargs)
  (declare (ignore initargs))
  'persistent-effective-slot-definition)

(defmethod compute-effective-slot-definition :around ((class persistent-class) slot-name direct-slot-definitions)
  (let ((slot (call-next-method)))
    (setf (slot-value slot 'serializable)
          (some #'slot-serializable-p direct-slot-definitions))
    slot))

(defclass persistent-object (serializable-object)
  ()
  (:documentation "Root of persistent objects.")
  (:metaclass persistent-class))

;;;; Persistence

(defvar *server-config*)
(defvar *db-name*)
(defvar *mongo-client*)
(defvar *db*)

(defun connect (database &rest server-config &key hostname port)
  "Sets the server parameters and creates a mongo connection. Reconnection is automatic in
  case of network failure."
  (declare (ignore hostname port))
  (setf *server-config* (apply #'make-instance 'mongo:server-config server-config))
  (setf *db-name* database)
  (reconnect)
  (values))

(defun reconnect ()
  (ignore-errors
    (mongo-cl-driver.adapters:mongo-client-close *mongo-client*))
  (setf *mongo-client* (make-instance 'mongo.usocket:mongo-client :server *server-config*))
  (setf *db* (make-instance 'mongo:database
                            :name *db-name*
                            :mongo-client *mongo-client*)))

(defmethod initialize-instance :after ((obj persistent-object) &key &allow-other-keys)
  "Stores newly created persistent object in the database"
  (let ((collection (mongo:collection *db* (string (type-of obj))))
        (ht (serialize obj :hashtable)))
    (mongo:insert-op collection ht)))

(defvar *in-transaction* nil)

(defmacro with-transaction (&body body)
  "All modified objects will be sent back to mongo when the transaction exits normally"
  `(if *in-transaction*
       (cerror "Already in transaction")
       (catch 'rollback-transaction
         (let ((*in-transaction* t))
           (values
            (prog1
                (progn
                  ,@body)
              (commit-transaction))
            t)))))

(defmethod (setf slot-value-using-class) :around (new-value (class persistent-class) obj (slot persistent-effective-slot-definition))
  (if (or *in-transaction*
          (eq (slot-definition-name slot) 'key))
      (call-next-method)
      (cerror "CONTINUE" "Attempt to modify a slot of persistent object while not inside a transaction")))

(defun commit-transaction ()
  (unless *in-transaction*
    (cerror "CONTINUE" "Attempt to rollback while not inside transaction")
    (return-from commit-transaction))
  nil)

(defun rollback-transaction ()
  (if *in-transaction*
      (throw 'rollback-transaction (values nil nil))
      (cerror "CONTINUE" "Attempt to rollback while not inside transaction")))

(defun getobj (class key)
  (let* ((collection (mongo:collection *db* (string class)))
         (ht (mongo:find-one collection :query (son (string 'key) key))))
    (remhash "_id" ht)
    (deserialize ht :hashtable)))

;;;; Utils

(defun symbol-fqn (symbol)
  (concatenate 'string (package-name (symbol-package symbol)) "::" (symbol-name symbol)))

(let ((counter (make-array 1 :initial-element 1 :element-type 'sb-ext:word)))
  (defun next-autokey ()
    (let ((new-counter (sb-ext:atomic-incf (aref counter 0))))
      (princ-to-string
       (+ new-counter
          (* (get-universal-time)
             (expt 10 (1+ (truncate (log new-counter 10))))))))))
