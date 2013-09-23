
(defsystem eshop-test
  :pathname "t"
  :serial t
  :components ((:file "package")
               (:file "avail")
               (:file "servo")
               (:file "odm-serialize")
               (:file "odm-persist"))
  :depends-on (:sb-cover
               :drakma
               :lift
               ))
