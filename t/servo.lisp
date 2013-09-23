(in-package eshop-test)


(deftestsuite servo (eshop-test)
  ()
  ;; parse-float
  (:tests
   (parse-float-1 (ensure-same (eshop::parse-float "54.") 54.0))
   (parse-float-2 (ensure-same (eshop::parse-float "54.0") 54.0))
   (parse-float-3 (ensure-same (eshop::parse-float "54") 54.0))
   (parse-float-4 (ensure-same (eshop::parse-float 54) 54)) ;;not float result!
   (parse-float-5 (ensure-same (eshop::parse-float "") 0))
   (parse-float-6 (ensure-same (eshop::parse-float nil) 0))
   ))
