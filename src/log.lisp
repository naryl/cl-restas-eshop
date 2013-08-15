;;;; log.lisp

(in-package #:eshop)

(log:config :sane ; Configure appenders
            :this-console ; Always print to the server console too
            :nopretty ; Avoid splitting log lies
            :time ; Include time in log messages
            :nofile ; Don't include source file name
            :daily (merge-pathnames "eshop.log" (config.get-option :paths :path-to-logs)) ;; requests log
            :backup nil ;; just one plain file, without rolling
            )

(defvar *eshop-access-log-lock* (bt:make-lock "eshop-request-log-lock"))
