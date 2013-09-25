
(in-package eshop-test)

(declaim (optimize (debug 3) (safety 3)))

(deftestsuite odm-serialize (eshop-test) ())

(defclass serializable-invert (eshop.odm:serializable-object)
  ((int :initarg :int
        :serializable t)
   (str :initarg :str
        :serializable t))
  (:metaclass eshop.odm:serializable-class))

(addtest invert-ht
  (let* ((a (make-instance 'serializable-invert :int 42 :str "Hi!"))
         (ht (eshop.odm:serialize a))
         (b (eshop.odm:deserialize ht)))
    (flet ((slot-equal (obja objb slot)
             (equal (slot-value obja slot)
                    (slot-value objb slot))))
      (ensure (slot-equal a b 'int))
      (ensure (slot-equal a b 'str)))))

(defclass serializable-special (eshop.odm:serializable-object)
  ((transient :initarg :transient)
   (unbound :serializable t))
  (:metaclass eshop.odm:serializable-class))

(addtest special-slots
  (let* ((a (make-instance 'serializable-special :transient :dummy))
         (ht (eshop.odm:serialize a))
         (b (eshop.odm:deserialize ht)))
    (flet ((slot-equal (obja objb slot)
             (equal (slot-value obja slot)
                    (slot-value objb slot))))
      (ensure (eql (slot-value a 'transient) :dummy))
      (ensure (not (slot-boundp b 'transient)))
      (ensure (not (slot-boundp a 'unbound)))
      (ensure (not (slot-boundp b 'unbound))))))
