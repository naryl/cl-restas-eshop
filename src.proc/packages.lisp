
(defpackage #:eshop.proc
  (:use :cl
        :sb-concurrency
        :bordeaux-threads)
  (:export #:process
           #:noproc
           #:process-running
           #:ensure-process
           #:stop-process
           #:process-call
           #:process-exec))
