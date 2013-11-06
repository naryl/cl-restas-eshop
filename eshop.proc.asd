
(defsystem eshop.proc
    :serial t
    :pathname "src.proc/"
    :components ((:file "packages")
                 (:file "proc"))
    :depends-on (sb-concurrency
                 let+
                 bordeaux-threads))
