
(in-package eshop-test)

(declaim (optimize (debug 3) (safety 3)))

(deftestsuite odm-persist (eshop-test)
  ()
  (:setup (eshop.odm:connect "zifry-test")))

(defclass persistent (eshop.odm:persistent-object)
  ((slot :serializable t :initarg :slot))
  (:metaclass eshop.odm:persistent-class))

(addtest create-delete
  (let* ((obj (make-instance 'persistent :slot 42))
         (key (eshop.odm:serializable-object-key obj))
         (store-obj (eshop.odm:getobj 'persistent key)))
    (ensure (eql key
                 (eshop.odm:serializable-object-key store-obj)))
    (ensure (eql (slot-value store-obj 'slot)
                 (slot-value obj 'slot)))
    (eshop.odm:remobj obj)
    (ensure (eq (eshop.odm:getobj 'persistent key) nil))))

(addtest print-persistent
  (let* ((obj (make-instance 'persistent))
         (str (princ-to-string obj)))
    (ensure (search (string 'persistent) str))
    (ensure (search "RO" str))
    (ensure (search (princ-to-string (eshop.odm:serializable-object-key obj)) str))
    (eshop.odm:remobj obj)
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
    (eshop.odm:remobj obj)))

(addtest remobj-in-transaction
  (let* ((obj (make-instance 'persistent :slot 42))
         (key (eshop.odm:serializable-object-key obj)))
    (eshop.odm:with-transaction
      (let ((store-obj (eshop.odm:getobj 'persistent key)))
        (eshop.odm:remobj store-obj)))
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
    (eshop.odm:remobj obj)))

(addtest rollback-remobj
  (let* ((obj (make-instance 'persistent :slot 42))
         (key (eshop.odm:serializable-object-key obj)))
    (eshop.odm:with-transaction
      (eshop.odm:remobj (eshop.odm:getobj 'persistent key))
      (eshop.odm:rollback-transaction))
    (ensure (eshop.odm:getobj 'persistent key))
    (eshop.odm:remobj obj)))

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
    (eshop.odm:remobj obj)))

(addtest nested-transaction
  (eshop.odm:with-transaction
    (eshop.odm:with-transaction
      (ensure (eq t t)))))

(addtest setobj
  (let* ((obj (make-instance 'persistent :slot 42))
         (key (eshop.odm:serializable-object-key obj)))
    (eshop.odm:setobj obj 'slot 43)
    (ensure (eq (slot-value (eshop.odm:getobj 'persistent key) 'slot) 43))
    (eshop.odm:remobj obj)))

(addtest mapobj
  (eshop.odm:mapobj #'eshop.odm:remobj 'persistent)
  (let ((objs (dotimes (i 10)
                (make-instance 'persistent :slot i))))
    (ensure (equal (alexandria:iota 10)
                   (eshop.odm:mapobj #'(lambda (o)
                                         (slot-value o 'slot))
                                     'persistent))))
  (eshop.odm:mapobj #'eshop.odm:remobj 'persistent))

(addtest doobj
  (eshop.odm:mapobj #'eshop.odm:remobj 'persistent)
  (let ((obj (dotimes (i 10)
                (make-instance 'persistent :slot i))))
    (ensure (equal (alexandria:iota 10)
                   (nreverse
                    (eshop.odm:doobj (obj 'persistent result)
                      (push (slot-value obj 'slot) result))))))
  (eshop.odm:doobj (obj 'persistent)
    (eshop.odm:remobj obj)))

(addtest errors
 (let ((obj (make-instance 'persistent :slot 42)))
   (ensure-error (setf (slot-value obj 'slot) 43))
    (eshop.odm:remobj obj)
    (ensure-error (slot-value obj 'slot)))
 (ensure-error (eshop.odm:rollback-transaction))
 (ensure-error (remobj 42))
 (ensure-error (remobj 'persistent)))
