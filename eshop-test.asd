
(defsystem eshop-test
  :pathname "t"
  :serial t
  :components ((:file "package")
               (:file "avail"))
  :depends-on (:drakma
               :lift
               ))
