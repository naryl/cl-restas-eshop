
(in-package eshop-test)

(deftestsuite odm-persist (eshop-test)
  ()
  (:run-setup :once-per-suite)
  (:setup (eshop.odm:connect "zifry-test")
          (dolist (coll (mapcar #'eshop.odm::symbol-fqn
                                '(persistent container
                                  item inline-item
                                  indexed versioned
                                  updates)))
            (mongo:drop-collection eshop.odm::*db* coll)
            (function-cache:clear-cache-all-function-caches))))

(defclass persistent (eshop.odm:persistent-object)
  ((slot :serializable t :initarg :slot :reader persistent-slot))
  (:metaclass eshop.odm:persistent-class))

(addtest create-delete
  (let* ((obj (make-instance 'persistent :slot 42))
         (key (eshop.odm:serializable-object-key obj))
         (store-obj (eshop.odm:getobj 'persistent key)))
    (ensure (eql key
                 (eshop.odm:serializable-object-key store-obj)))
    (ensure (eql (slot-value store-obj 'slot)
                 (slot-value obj 'slot)))
    (eshop.odm::remobj obj)
    (ensure (eq (eshop.odm:getobj 'persistent key) nil))))

(addtest print-persistent
  (let* ((obj (make-instance 'persistent))
         (str (princ-to-string obj)))
    (ensure (search (string 'persistent) str))
    (ensure (search "RO" str))
    (ensure (search (princ-to-string (eshop.odm:serializable-object-key obj)) str))
    (eshop.odm::remobj obj)
    (ensure (search "DELETED" (princ-to-string obj)))))

(addtest modify
  (let* ((obj (make-instance 'persistent :slot 42))
         (key (eshop.odm:serializable-object-key obj)))
    (eshop.odm:with-transaction
      (let ((store-obj (eshop.odm:getobj 'persistent key)))
        (ensure (eql (slot-value store-obj 'slot) 42))
        (setf (slot-value store-obj 'slot) 43)))
    (ensure (eql (slot-value (eshop.odm:getobj 'persistent key) 'slot)
                 43))
    (eshop.odm::remobj obj)))

(addtest remobj-in-transaction
  (let* ((obj (make-instance 'persistent :slot 42))
         (key (eshop.odm:serializable-object-key obj)))
    (eshop.odm:with-transaction
      (let ((store-obj (eshop.odm:getobj 'persistent key)))
        (eshop.odm::remobj store-obj)
        (ensure-error (setf (slot-value store-obj 'slot) 43))
        (ensure-error (slot-value store-obj 'slot))))
    (ensure (eq (eshop.odm:getobj 'persistent key) nil))))

(addtest rollback
  (let* ((obj (make-instance 'persistent :slot 42))
         (key (eshop.odm:serializable-object-key obj)))
    (eshop.odm:with-transaction
      (let ((store-obj (eshop.odm:getobj 'persistent key)))
        (ensure (eql (slot-value store-obj 'slot) 42))
        (setf (slot-value store-obj 'slot) 43)
        (eshop.odm:rollback-transaction)))
    (ensure (eql (slot-value (eshop.odm:getobj 'persistent key) 'slot)
                 42))
    (eshop.odm::remobj obj)))

(addtest rollback-remobj
  (let* ((obj (make-instance 'persistent :slot 42))
         (key (eshop.odm:serializable-object-key obj)))
    (eshop.odm:with-transaction
      (eshop.odm::remobj (eshop.odm:getobj 'persistent key))
      (eshop.odm:rollback-transaction))
    (ensure (eshop.odm:getobj 'persistent key))
    (eshop.odm::remobj obj)))

(addtest rollback-makeobj
  (let (key)
    (eshop.odm:with-transaction
      (let ((obj (make-instance 'persistent :slot 42)))
        (setf key (eshop.odm:serializable-object-key obj)))
      (eshop.odm:rollback-transaction))
    (ensure (eq (eshop.odm:getobj 'persistent key) nil))))

(addtest double-getobj
  (let* ((obj (make-instance 'persistent :slot 42))
         (key (eshop.odm:serializable-object-key obj)))
    (eshop.odm:with-transaction
      (let ((store-obj (eshop.odm:getobj 'persistent key)))
        (setf (slot-value store-obj 'slot) 43)
        (ensure (eql (slot-value (eshop.odm:getobj 'persistent key) 'slot) 43))))
    (eshop.odm::remobj obj)))

(addtest nested-transaction
  (eshop.odm:with-transaction
    (eshop.odm:with-transaction
      (ensure (eq t t)))))

(addtest setobj
  (let* ((obj (make-instance 'persistent :slot 42))
         (key (eshop.odm:serializable-object-key obj)))
    (eshop.odm:setobj obj 'slot 43)
    (ensure (eq (slot-value (eshop.odm:getobj 'persistent key) 'slot) 43))
    (eshop.odm::remobj obj)))

(addtest mapobj
  (eshop.odm:mapobj #'eshop.odm::remobj 'persistent)
  (let ((objs (dotimes (i 10)
                (make-instance 'persistent :slot i))))
    (ensure (equal (alexandria:iota 10)
                   (eshop.odm:mapobj #'(lambda (o)
                                         (slot-value o 'slot))
                                     'persistent))))
  (eshop.odm:mapobj #'eshop.odm::remobj 'persistent))

(addtest doobj
  (eshop.odm:mapobj #'eshop.odm::remobj 'persistent)
  (let ((obj (dotimes (i 10)
                (make-instance 'persistent :slot i))))
    (ensure (equal (alexandria:iota 10)
                   (nreverse
                    (eshop.odm:doobj (obj 'persistent result)
                      (push (slot-value obj 'slot) result))))))
  (eshop.odm:doobj (obj 'persistent)
    (eshop.odm::remobj obj)))

(addtest errors
 (let ((obj (make-instance 'persistent :slot 42)))
   (ensure-error (setf (slot-value obj 'slot) 43))
   (eshop.odm::remobj obj)
   (ensure-error (slot-value obj 'slot)))
 (ensure-error (eshop.odm:rollback-transaction))
 (ensure-error (eshop.odm::remobj 42))
 (ensure-error (eshop.odm::remobj 'persistent)))

(defclass container (eshop.odm:persistent-object)
  ((item :initarg :item
         :serializable t))
  (:metaclass eshop.odm:persistent-class))

(defclass item (eshop.odm:persistent-object)
  ()
  (:metaclass eshop.odm:persistent-class))

(defclass inline-item (eshop.odm:serializable-object)
  ()
  (:metaclass eshop.odm:serializable-class))

(addtest ref
  (function-cache:clear-cache 'eshop.odm::*getobj-cache-cache*)
  (let ((container (make-instance 'container
                                  :item (make-instance 'item))))

    (let ((container
           (eshop.odm:getobj 'container
                             (eshop.odm:serializable-object-key container))))
      (ensure (= 1 (function-cache:cached-results-count
                    eshop.odm::*getobj-cache-cache*)))
      (ensure
       (slot-value (eshop.odm:getobj 'container (eshop.odm:serializable-object-key container))
                   'item))
      (ensure (= 2 (function-cache:cached-results-count
                    eshop.odm::*getobj-cache-cache*))))))

(addtest inline-obj
  (function-cache:clear-cache 'eshop.odm::*getobj-cache-cache*)
  (let ((container (make-instance 'container
                                  :item (make-instance 'inline-item))))

    (let ((container
           (eshop.odm:getobj 'container
                             (eshop.odm:serializable-object-key container))))
      (ensure (= 1 (function-cache:cached-results-count
                    eshop.odm::*getobj-cache-cache*)))
      (ensure
       (slot-value (eshop.odm:getobj 'container (eshop.odm:serializable-object-key container))
                   'item))
      (ensure (= 1 (function-cache:cached-results-count
                    eshop.odm::*getobj-cache-cache*))))))

(defclass indexed (eshop.odm:persistent-object)
  ((key :initarg :local-key
        :index t
        :serializable t)
   (value :initarg :value
          :serializable t))
  (:metaclass eshop.odm:persistent-class))

(addtest get-one
  (let ((obj (make-instance 'indexed :local-key 1 :value 2)))
    (ensure (eql (slot-value (eshop.odm:get-one 'indexed (mongo.sugar:son 'key 1)) 'value)
                 2))))

(addtest get-list
  (let ((objs (mapcar #'(lambda (slot)
                          (make-instance 'persistent :slot slot))
                      '(1 2 3 4 5))))
    (ensure-error (eshop.odm:get-list 'persistent))
    (ensure (equal '(3)
                   (mapcar #'persistent-slot
                           (eshop.odm:get-list 'persistent
                                               :query (son 'slot 3)))))
    (ensure (equal '(1 2 3 4 5)
                   (mapcar #'persistent-slot
                           (eshop.odm:get-list 'persistent
                                               :limit 100))))
    (ensure (equal '(1 2 3)
                   (mapcar #'persistent-slot
                           (eshop.odm:get-list 'persistent
                                               :limit 3
                                               :sort (son 'slot 1)))))
    (ensure (equal '(2 3 4)
                   (mapcar #'persistent-slot
                           (eshop.odm:get-list 'persistent
                                               :limit 3
                                               :skip 1
                                               :sort (son 'slot 1)))))
    (ensure (equal '(4 3 2)
                   (mapcar #'persistent-slot
                           (eshop.odm:get-list 'persistent
                                               :limit 3
                                               :skip 1
                                               :sort (son 'slot -1)))))
    (eshop.odm:remobj objs)))

(addtest get-list-by-link
  (let* ((item (make-instance 'item))
         (containers (loop :repeat 5 collect (make-instance 'container :item item)))
         (store-objs (eshop.odm:get-list 'container :query (son 'item item)))
         (count (length store-objs)))
    (ensure (eql 5 count))))

(defclass versioned (eshop.odm:persistent-object)
  ((value :initarg :value
          :serializable t
          :versioned t))
  (:metaclass eshop.odm:persistent-class))

(addtest versions
  (let* ((obj (make-instance 'versioned :value 1))
         (key (eshop.odm:serializable-object-key obj)))
    (sleep 1.1)
    (eshop.odm:setobj obj 'value 2)
    (sleep 1.1)
    (let ((timestamp (get-universal-time)))
      (sleep 1.1)
      (eshop.odm:setobj obj 'value 3)
      (ensure (equalp (mapcar #'cdr (eshop.odm:object-slot-history
                                     'versioned key 'value))
                      '(1 2 3)))
      (ensure (eql (slot-value (eshop.odm:getobj 'versioned key :time timestamp)
                               'value)
                   2))
      (ensure (eql (slot-value (eshop.odm:getobj 'versioned key)
                               'value)
                   3)))))

(addtest update-class
  (defclass updates (eshop.odm:persistent-object)
    ((slot1 :initarg :slot1 :serializable t))
    (:metaclass eshop.odm:persistent-class))
  (let ((obj (make-instance 'updates :slot1 42)))
    (defclass updates (eshop.odm:persistent-object)
      ((slot2 :initform 43 :initarg :slot2 :serializable t))
      (:metaclass eshop.odm:persistent-class))
    (let ((obj2 (eshop.odm:getobj 'updates (eshop.odm:serializable-object-key obj))))
      (ensure (eql 43 (slot-value obj2 'slot2))))
    (eshop.odm:remobj obj)))

(addtest update-class-from-db
  (defclass updates (eshop.odm:persistent-object)
    ((slot1 :initarg :slot1 :serializable t))
    (:metaclass eshop.odm:persistent-class))
  (let ((obj (make-instance 'updates :slot1 42)))
    (function-cache:clear-cache-all-function-caches)
    (defclass updates (eshop.odm:persistent-object)
      ((slot2 :initform 43 :initarg :slot2 :serializable t))
      (:metaclass eshop.odm:persistent-class))
    (let ((obj2 (eshop.odm:getobj 'updates (eshop.odm:serializable-object-key obj))))
      (ensure (eql 43 (slot-value obj2 'slot2))))
    (eshop.odm:remobj obj)))
