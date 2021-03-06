
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro db-eval (&body body)
    `(process-exec (*db-proc*)
       ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SERIALIZABLE

;;;; Metaclasses

(defvar *deserializing* nil)

(defclass serializable-class (standard-class)
  ()
  (:documentation "Metaclass for serializable objects"))

(defmethod validate-superclass ((sub serializable-class) (super standard-class))
  t)

(defclass serializable-direct-slot-definition (standard-direct-slot-definition)
  ((serializable :initarg :serializable
                 :type boolean
                 :initform nil
                 :reader slot-serializable-p)
   (validation :initarg :validation
               :type function
               :initform nil
               :reader slot-validation)))

(defclass serializable-effective-slot-definition (standard-effective-slot-definition)
  ((serializable :initarg :serializable
                 :type boolean
                 :initform nil
                 :reader slot-serializable-p)
   (validation :initarg :validation
               :type list
               :reader slot-validation)))

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
          (some #'slot-serializable-p direct-slot-definitions)
          (slot-value slot 'validation)
          (remove nil (mapcar (compose #'eval #'slot-validation)
                              direct-slot-definitions)))
    slot))

(define-condition validation-error (error)
  ((obj :initarg :obj)
   (slot :initarg :slot)
   (value :initarg :value)
   (cause :initarg :cause))
  (:report (lambda (condition stream)
             (with-slots (obj slot value cause) condition
               (format stream "Validation error in class ~A's slot ~A~%  Value: ~S~%  Cause: ~S"
                       (class-of obj)
                       (slot-definition-name slot)
                       value
                       cause)))))

(defun validate-serializable-slot-change (slot new-value)
  (every #'(lambda (func)
             (funcall func new-value))
         (slot-validation slot)))

(defmethod (setf slot-value-using-class) :around
    (new-value (class serializable-class) obj (slot serializable-effective-slot-definition))
  (unless *deserializing*
    (flet ((validation-error (e)
             (error 'validation-error
                    :obj obj
                    :slot slot
                    :value new-value
                    :cause e)))
      (handler-bind ((error #'validation-error))
        (unless (validate-serializable-slot-change slot new-value)
          (validation-error nil)))))
  (call-next-method))

(defclass serializable-object ()
  ((key :initarg :key
        :serializable t
        :initform (unless *deserializing* (next-autokey))
        :reader serializable-object-key))
  (:documentation "Root of serializable objects.")
  (:metaclass serializable-class))

;;;; Serialization

(define-constant +class-key+ "%CLASS%" :test #'equal)

(defstruct obj-link
  class
  key)

(defgeneric serialize (obj)
  (:documentation "Serializes OBJ to hashtable.
  SERIALIZE and DESERIALIZE are inverse. If you successfully serialized an object
  deserializing it should always yield exact same object.")
  (:method ((obj serializable-object))
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
                    (serialize-slot (slot-value obj slot-name)))))))
      ht)))

(defgeneric serialize-slot (obj)
  (:method ((obj serializable-object))
    (serialize obj))
  (:method ((string string))
    string)
  (:method ((number number))
    number)
  (:method ((true (eql t)))
    t)
  (:method ((false (eql nil)))
    nil)
  (:method ((symbol symbol))
    (son "%TYPE%" ":SYMBOL"
         "%VALUE%" (symbol-fqn symbol)))
  (:method ((list list))
    (mapcar #'serialize-slot list)))

(defun write-link (class key)
  (son "%TYPE%" ":LINK"
       "%CLASS%" (symbol-fqn class)
       "%KEY%" (serialize-slot key)))

(defgeneric deserialize (data)
  (:documentation "Deserializes object from DATA using SOURCE data structure.
  SERIALIZE and DESERIALIZE are inverse. If you successfully serialized an object
  deserializing it should always yield exact same object.")
  (:method ((ht hash-table))
    (let ((*deserializing* t))
      (deserialize-ht ht (read-from-string (gethash "%TYPE%" ht ":OBJ")))))
  (:method ((string string))
    string)
  (:method ((number number))
    number)
  (:method ((true (eql t)))
    t)
  (:method ((false (eql nil)))
    nil)
  (:method ((list list))
    (mapcar #'deserialize list)))

(defgeneric deserialize-ht (ht class)
  (:method (ht (class (eql :link)))
    (make-obj-link :class (fqn-symbol (gethash "%CLASS%" ht))
                   :key (gethash "%KEY%" ht)))
  (:method (fqn (class (eql :symbol)))
    (fqn-symbol (gethash "%VALUE%" fqn)))
  (:method (ht (class (eql :obj)))
    (let* ((class-name (fqn-symbol (gethash +class-key+ ht)))
           (class (find-class class-name)))
      (make-instance class 'ht ht))))

(defun set-slots-from-ht (instance ht)
  (maphash #'(lambda (k v)
               (unless (string= k +class-key+)
                 (let ((slot-name (fqn-symbol k)))
                   (if (slot-exists-p instance slot-name)
                       (setf (slot-value instance slot-name)
                             (deserialize v))
                       (warn "Slot ~A missing while deserializing ~A"
                             slot-name instance)))))
           ht))

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
              :reader slot-versioned)
   (index :initform nil
          :initarg :index
          :reader slot-index)))

(defclass persistent-effective-slot-definition (serializable-effective-slot-definition)
  ((versioned :initform nil
              :type boolean
              :initarg :versioned
              :reader slot-versioned)
   (modified :initform nil
             :type (or boolean symbol)
             :accessor slot-modified)
   (index :initform nil
          :initarg :index
          :reader slot-index)))

(defmethod slot-versioned ((slot standard-direct-slot-definition))
  (declare (ignore slot))
  nil)

(defmethod slot-index ((slot standard-direct-slot-definition))
  (declare (ignore slot))
  nil)

(defmethod slot-index ((slot standard-effective-slot-definition))
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
    (let* ((asc-index (find :asc direct-slot-definitions :key #'slot-index))
           (desc-index (find :desc direct-slot-definitions :key #'slot-index))
           (both-index (or (find :both direct-slot-definitions :key #'slot-index)
                           (and asc-index desc-index))))
      (setf (slot-value slot 'versioned) versioned)
      (setf (slot-value slot 'index) (cond (asc-index :asc)
                                           (desc-index :desc)
                                           (both-index :both)))
      slot)))

(defclass persistent-object (serializable-object)
  ((key :initarg :key
        :serializable t
        :index :asc
        :initform (unless *deserializing* (next-autokey))
        :reader serializable-object-key)
   (state :initform :rw
          :type (member :rw :ro :deleted)
          :accessor persistent-object-state)
   (modified :initform nil
             :type boolean
             :accessor persistent-object-modified))
  (:documentation "Root of persistent objects.")
  (:metaclass persistent-class))

(defmethod print-object :around ((obj persistent-object) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (if (and (slot-boundp obj 'key)
             (slot-boundp obj 'state))
        (case (persistent-object-state obj)
          ((:rw :ro)
           (format stream "~A ~S"
                   (persistent-object-state obj)
                   (serializable-object-key obj))
           (let ((inner (with-output-to-string (out)
                          (call-next-method obj out))))
             (unless (equal inner "")
               (format stream " ~A" inner))))
          ((:deleted) (format stream "DELETED")))
        (format stream "DUMMY"))))

(defmethod print-object ((obj persistent-object) stream)
  nil)

(defmethod serialize-slot ((obj persistent-object))
  (write-link (type-of obj) (serializable-object-key obj)))

(defmethod shared-initialize :around ((instance serializable-object) slots
                                      &rest initargs
                                      &key ((ht ht)))
  (cond (*deserializing*
         (when ht
           (set-slots-from-ht instance ht))
         (when (eq slots t)
           (setf slots (mapcar #'slot-definition-name
                               (class-slots (class-of instance)))))
         (apply #'call-next-method
                instance
                (remove-if #'(lambda (slot-name)
                               (slot-boundp instance slot-name))
                           slots)
                initargs)
         (when (typep instance 'persistent-object)
           (setf (slot-value instance 'modified) nil)))
        (t (call-next-method))))

(defmethod update-instance-for-redefined-class :around ((instance persistent-object)
                                                        added-slots discarded-slots
                                                        property-list &rest initargs)
  (declare (ignore instance added-slots discarded-slots property-list initargs))
  (let ((*deserializing* t))
    (call-next-method)))

;;;; Persistence

(defvar *server-config*)
(defvar *db-name*)
(defvar *mongo-client*)
(defvar *db*)

(defvar *db-proc* (make-instance 'process :name "DB"))

(defvar *transaction* nil)

(deftimer (cache-cleaner 60 5)
  (purge-all-caches))

(defun connect (database &rest server-config &key hostname port)
  "Sets the server parameters and creates a mongo connection. Reconnection is automatic in
  case of network failure."
  (declare (ignore hostname port))
  (setf *server-config* (apply #'make-instance 'mongo:server-config server-config))
  (setf *db-name* database)
  (start-timer cache-cleaner)
  (reconnect)
  (ensure-indexes)
  (values))

(defun reconnect ()
  (log:info "Connecting to mongo...")
  (unless (and *db-name* *server-config*)
    (error "Can't reconnect before making initial connection using CONNECT"))
  (when (process-running *db-proc*)
    (stop-process *db-proc*))
  (ignore-errors
    (mongo-cl-driver.adapters:mongo-client-close *mongo-client*))
  (setf *mongo-client* (make-instance 'mongo.usocket:mongo-client :server *server-config*))
  (setf *db* (make-instance 'mongo:database
                            :name *db-name*
                            :mongo-client *mongo-client*))
  (ensure-process *db-proc*))

(defun ensure-indexes ()
  (let ((cnt 0))
    (dolist (class (list-persistent-classes))
      (c2mop:finalize-inheritance class)
      (db-eval
        (let ((collection (obj-collection (class-name class))))
          (dolist (slot (class-slots class))
            (flet ((make-index (dir)
                     (let ((index-name (symbol-fqn (slot-definition-name slot))))
                       (log:info "Ensuring index over class ~A; slot: ~A"
                                 (symbol-fqn (class-name class))
                                 index-name)
                       (incf cnt)
                       (mongo:ensure-index collection
                                           (son index-name dir)))))
              (case (slot-index slot)
                (nil nil)
                (:asc (make-index 1))
                (:desc (make-index -1))
                (:both (make-index 1)
                       (make-index -1))))))))
    (log:info "~A indexes ensured" cnt)))

(defmethod initialize-instance :before ((obj persistent-object) &key &allow-other-keys)
  (setf (slot-value obj 'state) :rw))

(defmethod initialize-instance :around ((obj persistent-object) &key &allow-other-keys)
  "Stores newly created persistent object in the database"
  (call-next-method)
  (cond (*deserializing* ; Fetching a persistent object
         (setf (slot-value obj 'state) :ro))
        (t ; Actually making a new one
         (if *transaction*
             (setf (slot-value obj 'modified) t)
             (update-instance obj))
         (new-transaction-object obj))))

(defun delete-instance (obj)
  (getobj-remcache obj)
  (db-eval
    (mongo:delete-op (obj-collection obj)
                     (son (symbol-fqn 'key) (serializable-object-key obj)))))

(defun update-instance (obj)
  (getobj-remcache obj)
  (let ((class (class-of obj)))
    (let ((ht (serialize obj))
          (collection (obj-collection obj))
          (version-collection (obj-collection obj t))
          (slot-changes (when (persistent-class-versioned class)
                          (collect-slot-changes obj))))
      (db-eval
        (mongo:update-op collection
                         (son (symbol-fqn 'key)
                              (serializable-object-key obj))
                         ht
                         :upsert t)
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
                     (son (symbol-fqn 'key) (serializable-object-key obj)
                          "TIMESTAMP" time
                          "SLOT" (symbol-fqn slot-name)
                          "VALUE" (serialize-slot (slot-value obj slot-name)))))))
            (class-slots (class-of obj)))))

(defvar *finished* nil)

(defmacro with-transaction (&body body)
  "All modified objects will be sent back to mongo when the transaction exits normally"
  (with-gensyms (transaction)
    `(flet ((,transaction ()
              ,@body))
       (let ((*finished* nil))
         (if *transaction*
             (,transaction)
             (catch 'rollback-transaction
               (let ((*transaction* (make-hash-table :test #'equal)))
                 (values
                  (unwind-protect
                       (prog1
                           (,transaction)
                         (commit-transaction)
                         (setf *finished* t))
                    (unless *finished*
                      (setf *finished* t) ; non-local exit
                      (rollback-transaction)))
                  t))))))))

(defmethod slot-value-using-class :around
    ((class persistent-class) obj (slot persistent-effective-slot-definition))
  (let ((slot-name (slot-definition-name slot)))
    (if (or (member slot-name
                    '(state key modified))
            (not (eq :deleted (persistent-object-state obj))))
        (let ((value (call-next-method)))
          (if (typep value 'obj-link)
              (getobj (obj-link-class value)
                      (obj-link-key value))
              value))
        (error "Attempt to access a slot of deleted persistent object"))))

(defun allow-modifying-slot (obj slot)
  (or (find (slot-definition-name slot) '(state modified))
      (not (slot-boundp obj 'state))
      (eq :rw (persistent-object-state obj))))

(defun slot-changed (new-value obj slot)
  (let ((slot-name (slot-definition-name slot)))
    (and (slot-serializable-p slot)
         (or (and (slot-boundp obj slot-name)
                  (not (equal new-value (slot-value obj slot-name))))
             (not (slot-boundp obj slot-name))))))

(defmethod (setf slot-value-using-class) :around
    (new-value (class persistent-class) obj (slot persistent-effective-slot-definition))
  (unless *deserializing*
    (cond ((not (allow-modifying-slot obj slot))
           (error "Attempt to modify a slot of read-only persistent object"))
          ((slot-changed new-value obj slot)
           (setf (persistent-object-modified obj) t)
           (setf (slot-modified slot) t))))
  (call-next-method))

(defcached (getobj-cache :timeout 600) (class key)
  (fetch-object class key))

(defun getobj-remcache (obj)
  (clear-cache-partial-arguments *getobj-cache-cache*
                                 (list (type-of obj)
                                       (serializable-object-key obj))))

(defun fetch-object (class key)
  (let ((ht (let ((collection (obj-collection class)))
              (db-eval
                (mongo:find-one collection
                                :query (son (symbol-fqn 'key) key)
                                :selector (son "_id" 0))))))
    (when (and ht
               (string= (symbol-fqn class)
                        (gethash +class-key+ ht)))
      ht)))

(defun commit-transaction ()
  (unless *transaction*
    (error "Attempt to commit while not inside transaction"))
  (maphash #'(lambda (key obj)
               (declare (ignore key))
               (when (persistent-object-modified obj)
                 (case (persistent-object-state obj)
                   (:rw
                    (setf (persistent-object-state obj) :ro)
                    (update-instance obj))
                   (:deleted
                    (delete-instance obj)))))
           *transaction*)
  (values))

(defun rollback-transaction ()
  "Invalidates all transaction objects. Doesn't modify the DB."
  (cond (*transaction*
         (maphash #'(lambda (key obj)
                      (declare (ignore key))
                      (getobj-remcache obj)
                      (setf (persistent-object-state obj) :deleted))
                  *transaction*)
         (unless *finished*
           (setf *finished* t)
           (throw 'rollback-transaction (values nil nil))))
        (t
         (error "Attempt to rollback while not inside transaction"))))

(defun new-transaction-object (obj)
  (when obj
    (setf (persistent-object-state obj)
          (cond (*transaction*
                 (setf (gethash (list (type-of obj)
                                      (serializable-object-key obj))
                                *transaction*)
                       obj)
                 :rw)
                (t
                 :ro)))
    obj))

(defun getobj (class key &key time)
  (if time
      (getobj-for-date class key time)
      (getobj-current class key)))

(defun getobj-current (class key)
  "Fetch an object from the database. The object will be read-only unless it's done inside
  a transaction."
  (metric:count "getobj")
  (if *transaction*
      (awhen (gethash (list class key) *transaction*)
        (return-from getobj-current it)))
  (when-let* ((db-obj (getobj-cache class key))
              (obj (deserialize db-obj)))
    (new-transaction-object obj)))

(defun remobj (&rest objs)
  "Deletes an object from the database"
  (setf objs (flatten (ensure-list objs))) ; Ensure OBJS is a flat list of objects
  (dolist (obj objs)
    (let ((class (type-of obj))
          (key (serializable-object-key obj)))
      (setf (persistent-object-state obj) :deleted)
      (if *transaction*
          (if (gethash (list class key) *transaction*)
              (setf (persistent-object-modified obj) t)
              (remobj (getobj class key)))
          (delete-instance obj)))))

(defun setobj (obj-or-class &rest slots-values)
  "Modifies an object in a transaction.
  Call either as (setobj (getobj 'class 1234) 'a 1 'b 2)
  or (setobj 'class 1234 'a 1 'b 2)"
  (unless obj-or-class
    (error "Can't do SETOBJ for NIL"))
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

(defun instance-count (class &key query)
  (db-eval
    (truncate (mongo:$count (obj-collection class)
                            (when query (cook-query query))))))

(defun list-persistent-classes ()
  "Finds all subclasses including indirect"
  (let ((classes ()))
    (labels ((walk-classes (class)
               (push class classes)
               (mapc #'walk-classes (class-direct-subclasses class))))
      (mapc #'walk-classes
            (class-direct-subclasses (find-class 'persistent-object))))))

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

(defun get-one (class query)
  "Fetches an object by mongo query and stores it in cache by key"
  (let* ((cooked-query (cook-query query))
         (key-ht (db-eval
                   (mongo:find-one (obj-collection class)
                                   :query cooked-query
                                   :selector (son (symbol-fqn 'key) 1
                                                  "_id" 0)))))
    (when key-ht
      (getobj class (gethash (symbol-fqn 'key) key-ht)))))

(defun get-list (class &key query sort skip limit)
  "Fetches all instances of a class missing cache (to avoid wiping it)"
  (unless (or query limit)
    (error "Require either QUERY or LIMIT in call to GET-LIST to avoid fetching the whole
collection"))
  (let* ((cooked-query (when query (cook-query query)))
         (cooked-sort (when sort (cook-query sort)))
         (pipeline (remove nil (list (when query (son "$match" cooked-query))
                                     (when sort (son "$sort" cooked-sort))
                                     (when skip (son "$skip" skip))
                                     (when limit (son "$limit" limit)))))
         (hts (db-eval
                (apply #'mongo:aggregate
                       (obj-collection class)
                       pipeline))))
    (mapcar #'(lambda (ht)
                (when (and ht
                           (string= (symbol-fqn class)
                                    (gethash +class-key+ ht)))
                  (remhash "_id" ht)
                  (deserialize ht)))
            hts)))

;;;; Version queries

(defun object-slot-history (class key slot)
  (let ((raw-data
         (db-eval
           (mongo:find-list (obj-collection class t)
                            :query (son (symbol-fqn 'key) key
                                        "SLOT" (symbol-fqn slot))
                            :fields (son "_id" 0
                                         "SLOT" 0)))))
    (sort (mapcar #'(lambda (obj)
                      (cons (unix-to-universal-time (gethash "TIMESTAMP" obj))
                            (gethash "VALUE" obj)))
                  raw-data)
          #'<
          :key #'car)))

(defun getobj-for-date (class key time)
  ;; Not using GETOBJ because we don't want a writable object here even in transaction
  ;; Not using GETOBJ-CACHE either because we need an uninterned copy so we can modify slots
  (let ((current-object (fetch-object class key))
        (version-collection (obj-collection class t)))
    (let ((actual-slots
           (mongo:aggregate version-collection
                            (son "$match" (son "TIMESTAMP"
                                               (son "$lte"
                                                    (universal-to-unix-time time))
                                               (symbol-fqn 'key) key))
                            (son "$sort" (son "TIMESTAMP" -1))
                            (son "$group" (son "_id" "$SLOT"
                                               "TIMESTAMP" (son "$first" "$TIMESTAMP")
                                               "VALUE" (son "$first" "$VALUE")))
                            (son "$project" (son "_id" 0
                                                 "SLOT" "$_id"
                                                 "VALUE" 1)))))
      (dolist (slot actual-slots)
        (setf (gethash (gethash "SLOT" slot) current-object)
              (gethash "VALUE" slot)))
      (deserialize current-object))))

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
    (symbol-fqn class))
  (:method ((obj persistent-object) &optional versions)
    (declare (ignore versions))
    (symbol-fqn (type-of obj)))
  (:method :around (obj &optional versions)
    (mongo:collection *db* (format nil "~A~:[~;_VERSIONS~]"
                                   (call-next-method)
                                   versions))))

(defun getobj-cache-size ()
  (cached-results-count *getobj-cache-cache*))

(metric:defmetric getobj-cache-size ("getobj.cache")
  (getobj-cache-size))

(defun cook-query (raw-query)
  (let ((cooked-query (son)))
    (maphash #'(lambda (k v)
                 (setf (gethash (symbol-fqn k) cooked-query)
                       (serialize-slot v)))
             raw-query)
    cooked-query))

;;;; Stuff

;; (defmethod print-object ((instance hash-table) stream)
;;   (format stream "#HT(~{~S~^ ~})" (hash-table-plist instance)))
