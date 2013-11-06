
(defpackage #:eshop.proc
  (:use :cl
        :let+
        :sb-concurrency
        :bordeaux-threads)
  #-package-local-nicknames-only
  (:nicknames :proc)
  (:export #:process
           #:noproc
           #:running-p
           #:ensure-running
           #:stop
           #:kill
           #:exec))
