
(in-package eshop-test)

(declaim (optimize (debug 3) (safety 3)))

(deftestsuite odm-serialize () ())

(defclass test ()
  ((int :initarg :int
        :serializable t)
   (str :initarg :str
        :serializable t)
   (transient :initarg :transient))
  (:metaclass eshop.odm:serializable-class))

(addtest invert
  (let* ((a (make-instance 'test :int 42 :str "Hi!" :transient :dummy))
         (ht (eshop.odm:serialize a :hashtable))
         (b (eshop.odm:deserialize ht :hashtable)))
    (flet ((slot-equal (obja objb slot)
             (equal (slot-value obja slot)
                    (slot-value objb slot))))
      (ensure (slot-equal a b 'int))
      (ensure (slot-equal a b 'str))
      (ensure (eql (slot-value a 'transient) :dummy))
      (ensure (not (slot-boundp b 'transient))))))
