
(defpackage #:eshop.odm
  (:use :c2cl
        :anaphora
        :alexandria
        :eshop.proc
        :function-cache
        :mongo-cl-driver.sugar)
  (:export #:serializable-class
           #:serializable-object
           #:serializable-object-key
           #:key
           #:serialize
           #:deserialize
           ;; ^^^^ Serialization
           ;; vvvv Persistence
           #:connect
           #:reconnect
           #:persistent-class
           #:persistent-object
           #:getobj
           #:get-one
           #:get-list
           #:setobj
           #:mapobj
           #:doobj
           #:instance-count
           #:list-persistent-classes
           ;; Versions
           #:getobj-for-date
           #:object-slot-history
           #:remobj
           #:with-transaction
           #:rollback-transaction
           ;; Validation
           #:validation-error
           ))
