(in-package eshop-test)


(deftestsuite json (eshop-test)
  ()
  ;;
  (:tests
   (patch-st-json-1 (ensure-same
                     (let ((ht (make-hash-table)))
                       (setf (gethash :|0| ht) 0)
                       (setf (gethash 0 ht) 0)
                       (setf (gethash t ht) "0")
                       (st-json:write-json-to-string ht))
                     "{\"0\":0,\"0\":0,\"T\":\"0\"}"))
   ))
