
(defsystem eshop-test
  :pathname "t"
  :serial t
  :components ((:file "package")
               (:file "avail")
               (:file "odm-serialize")
               (:file "odm-persist"))
  :depends-on (:drakma
               :lift
               ))
