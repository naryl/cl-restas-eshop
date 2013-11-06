
(in-package eshop-test)

(defmacro ensure-time ((sign time) &body body)
  (alexandria:with-gensyms (start-time result)
    `(let ((,start-time (get-internal-real-time))
           (,result (progn ,@body)))
       (ensure (,sign (get-internal-real-time)
                      (+ ,time ,start-time)))
       ,result)))

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
      (ensure-time (< 100)
          (proc:exec (proc :ignore) ()
            (sleep 1)))
      (ensure-time (> 900)
        (proc:stop proc))))
   (sync
    (let ((proc (get-proc)))
      (ensure (eql 42
                   (ensure-time (> 900)
                     (proc:exec (proc) ()
                       (sleep 1)
                       42))))
      (proc:stop proc)))
   (async
    (let ((proc1 (get-proc))
          (proc2 (get-proc)))
      (let ((f1 (ensure-time (< 100)
                  (proc:exec (proc1 :async) ()
                    (sleep 1)
                    (+ 1 2))))
            (f2 (ensure-time (< 100)
                  (proc:exec (proc2 :async) ()
                    (sleep 1)
                    (+ 3 4)))))
        (ensure (eql (ensure-time (> 900) (funcall f1)) 3))
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
