
(defsystem eshop.odm
  :serial t
  :pathname "src.odm/"
  :components ((:file "packages")
               (:file "util")
               (:file "odm"))
  :depends-on (eshop.proc
               function-cache
               anaphora
               alexandria
               closer-mop
               mongo-cl-driver
               mongo-cl-driver.usocket))
