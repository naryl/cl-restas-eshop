
(in-package eshop-test)

(deftestsuite proc (eshop-test)
  ()
  (:function
   (get-proc ()
             (let ((proc (make-instance 'proc:process)))
               (proc:ensure-running proc)
               proc)))
  (:tests
   (start
    (let ((proc (get-proc)))
      (proc:stop proc)))
   (ignored
    (let ((proc (get-proc)))
      (proc:exec (proc :ignore) ()
        42)
      (proc:stop proc)))
   (sync
    (let ((proc (get-proc)))
      (ensure (eql 42
                   (proc:exec (proc) ()
                     42)))
      (proc:stop proc)))
   (async
    (let ((proc1 (get-proc))
          (proc2 (get-proc)))
      (let ((f1 (proc:exec (proc1 :async) ()
                    (+ 1 2)))
            (f2 (proc:exec (proc2 :async) ()
                    (+ 3 4))))
        (ensure (eql (funcall f1) 3))
        (ensure (eql (funcall f2) 7)))
      (proc:stop proc1)
      (proc:stop proc2)))
   (error-sync
    (let ((proc (get-proc)))
      (ensure-error (proc:exec (proc) ()
                      (error 'error)))
      (proc:stop proc)))
   (error-async
    (let ((proc (get-proc)))
      (let ((f1 (proc:exec (proc :async) ()
                  (error 'error))))
        (ensure-error (funcall f1)))
      (proc:stop proc)))))
