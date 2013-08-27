
(defsystem eshop-test
  :pathname "t"
  :serial t
  :components ((:file "package")
               (:file "avail")
               (:file "odm"))
  :depends-on (:drakma
               :lift
               ))
