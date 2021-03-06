;;;; storage.lisp

(in-package #:eshop)

(defun get-storage (type)
  "Get storage for given type objects"
  (declare (symbol type))
  (getf (gethash type *classes*) :storage))

(defun getobj (key &optional type default)
  "Get object of given type from appropriate storage.
If no type given, search in all storages.
Note: returned object is NOT setfable (but its fields are)"
  (declare ((or null string) key) (symbol type))
  (metric:count "getobj")
  (when key
    (let ((cached (if type
                      (gethash key
                               (get-storage type)
                               default)
                      ;; else, search in all storages
                      (getobj-global key))))
      (or cached
          (getobj-from-db key type)))))

(defvar *db* (make-instance 'mongo:database
                            :name "zifry"
                            :mongo-client (make-instance 'mongo.usocket:mongo-client)))

(defun getobj-from-db (key &optional type)
  "Get object from mongo. If type is nil it will try all collections"
  nil ; Pending migration to mongo
  #+(or)(let ((types (if type
                    (list type)
                    (alexandria:hash-table-keys *classes*))))
    (dolist (type types)
      (awhen (mongo:find-one (mongo:collection *db* (string type))
                             :query (son "KEY" key))
        (let ((obj (%unserialize-from-hashtable type it)))
          (setobj (key obj) obj)
          (return-from getobj-from-db obj))))))

(defun getobj-global (key)
  "Get object regardless of type, from some storage (try to find in all)
Note: returned object is NOT setfable (but its fields are)"
  (declare ((or null string) key))
  (when key
    (let (res)
      (maphash #'(lambda (k v)
                   (declare (ignore v))
                   (awhen (and (not res)  ; find only first (but almost always
                                          ; there should be only one required object)
                               (get-storage k)
                               (gethash key (get-storage k)))
                     (setf res it)))
               *classes*)
      res)))

(defun setobj (key value &optional type)
  "Set/edit object of given type (type of value) in appropriate storage"
  (declare (string key) (symbol type))
  ;;; TODO: check key in object
  (let ((storage (get-storage (if type
                                  type
                                  (type-of value)))))
    (setf (gethash key storage) value)))

(defun editobj (value &optional type)
  "Edit obj in storage, if it's found, do nothing otherwise.
Key and type is accessed from element itself"
  (declare (symbol type))
  (let ((type (if type
                  type
                  (type-of value)))
        (key (key value)))
    (when (getobj key type)
      (setobj key value type))))


(defun remobj (key &optional type)
  "Get object of given type from appropriate storage
If no type given, search in all storages"
  (declare (string key) (symbol type))
  (remhash key (get-storage (if type
                                type
                                (type-of (getobj key))))))

(defun remobj-global (key)
  (maphash #'(lambda (k v)
               (declare (ignore v))
               (remhash key (get-storage k)))
           *classes*))

(defun process-storage (func type &optional (when-fn (constantly t)))
  "Process storage of given type apllying given func to each element.
Func should take 1 argument - elt for processing
Note: processed element can't be changed during processing"
  (declare (function func when-fn) (symbol type))
  (maphash #'(lambda (k v)
               (declare (ignore k))
               (when (funcall when-fn v)
                 (funcall func v)))
           (get-storage type)))

(defun process-storage-with-keys (func type)
  "Same as process-storage, but func should take 2 arguments - key and value"
  (declare (function func) (symbol type))
  (maphash func (get-storage type)))

(defun collect-storage (type &key (func #'identity) (when-fn #'identity))
  "Process storage of given type checking via when-fn, applying func to
each element and collecting its results"
  (loop
     :for elt :being :the hash-values :in (get-storage type)
     :when (funcall when-fn elt)
     :collect (funcall func elt)))

(defun count-storage (type &key (when-fn #'identity when-fn-supplied-p))
  "Count number of elements in storage, satisfying given function when-fn"
  (declare (symbol type) (function when-fn))
  (if when-fn-supplied-p
      (loop
         :for v :being :the hash-value :in (get-storage type)
         :count (funcall when-fn v))
      ;; else
      (hash-table-count (get-storage type))))


(defun get-root-groups ()
  "Return list of root groups sorted by order"
  (stable-sort
   (collect-storage 'group :when-fn (complement #'parents))
   #'menu-sort))

(defun storage.alphabet-group-sorter (a b)
  (when (and (name a) (name b))
    (string< (name a) (name b))))

(defun storage.get-recursive-products (group &optional (when-fn #'active))
  "Return list of all products of given group and all its child groups,
filtered by wgen-func"
  (declare (group group) (function when-fn))
  (append
   (remove-if-not when-fn (products group))
   (mapcan #'(lambda (gr)
               (storage.get-recursive-products gr when-fn))
           (groups group))))

(defun storage.count-recursive-products (group &optional (when-fn #'active))
  "Return count of all products of given group and all its child groups,
filtered by wgen-func"
  (declare (group group) (function when-fn))
  (+
   (count-if when-fn (products group))
   (apply #'+ (mapcar #'(lambda (gr)
               (storage.count-recursive-products gr when-fn))
           (groups group)))))

(defgeneric storage.get-vendors (object)
  (:documentation ""))

(defmethod storage.get-vendors ((object group))
  (storage.get-vendors (storage.get-recursive-products object)))

(defmethod storage.get-vendors ((object list))
  "By given product-list return hash table of vendors,
where key is vendor name and value is number of products with this vendor"
  (let ((vendors (make-hash-table :test #'equal)))
    (mapcar #'(lambda (product)
                (let ((vendor (vendor product)))
                  (when (valid-string-p vendor)
                    (sif (gethash vendor vendors)
                         (incf it)
                         (setf it 1)))))
            object)
    vendors))

(defun storage.get-all-sorted-child-groups (root &optional (sorter #'storage.alphabet-group-sorter) (when-fn #'identity))
    "Return all recursive child groups for root group using when-fn inspection starting from top group level & sort thems"
    (declare (group root) (function when-fn sorter))
    (sort (storage.get-all-child-groups root when-fn) sorter))

(defun storage.get-all-child-groups (root &optional (when-fn #'identity))
  "Return all recursive child groups for root group using when-fn inspection starting from top group level"
  (declare (group root) (function when-fn))
  (aif (remove-if-not when-fn (groups root))
       ;; no need for sort in mapcan, because it will be sorted anyway
       (mapcan #'(lambda (group) (storage.get-all-child-groups group when-fn)) it)
       (list root))) ; else


(defgeneric storage.get-filtered-products (object &optional filter)
  (:documentation "Returns list of prodicts taken from object somehow, and deleted all
products, which don't match filter function"))

(defmethod storage.get-filtered-products ((object group) &optional (filter #'active))
  (storage.get-recursive-products object filter))

(defmethod storage.get-filtered-products ((object list) &optional (filter #'active))
  (remove-if-not filter object))
