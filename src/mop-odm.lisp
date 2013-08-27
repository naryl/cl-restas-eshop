
;;;; mop-odm.lisp

(in-package eshop.odm)

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
  ()
  (:metaclass serializable-class))

(defmethod initialize-instance :around ((class serializable-class) &rest all-keys &key direct-superclasses)
  "Ensures SERIALIZABLE-OBJECT parent in instances of SERIALIZABLE-CLASS"
  (let ((root-class (find-class 'serializable-object))
        (metaclass (find-class 'serializable-class)))
    (if (member-if #'(lambda (super)
                       (eq (class-of super) metaclass))
                   direct-superclasses)
        (call-next-method)
        (apply #'call-next-method
               class
               :direct-superclasses (append direct-superclasses (list root-class))
               all-keys))))

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
            (symbol-fqn (class-name class)))
      (dolist (slot slots)
        (when (slot-serializable-p slot)
          (let ((slot-name (slot-definition-name slot)))
            (setf (gethash (symbol-fqn slot-name) ht)
                  (slot-value obj slot-name)))))
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
      (unserialize (st-json:read-json-from-string string) :hashtable)))
  (:method ((ht hash-table) (source (eql :hashtable)))
    (let* ((class-name (read-from-string (gethash +class-key+ ht)))
           (class (find-class class-name))
           (obj (make-instance class)))
      (maphash #'(lambda (k v)
                   (unless (string= k +class-key+)
                     (setf (slot-value obj (read-from-string k))
                           (deserialize v :hashtable))))
               ht)
      obj))
  (:method ((string string) (source (eql :hashtable)))
    string)
  (:method ((number number) (source (eql :hashtable)))
    number))

;;;; Utils

(defun symbol-fqn (symbol)
  (concatenate 'string (package-name (symbol-package symbol)) "::" (symbol-name symbol)))
