
(defpackage #:eshop.odm
  (:use :c2cl
        :anaphora
        :alexandria
        :function-cache
        :mongo-cl-driver.sugar)
  (:import-from :mongo #:duplicate-key-error)
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
           #:regetobj
           #:setobj
           #:mapobj
           #:doobj
           #:instance-count
           #:list-persistent-classes
           #:duplicate-key-error ; Reexport from mongo-cl-driver
           ;; Versions
           #:getobj-for-date
           #:object-slot-history
           #:remobj
           #:with-transaction
           #:rollback-transaction
           ;; Validation
           #:validation-error
           ))
