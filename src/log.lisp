;;;; log.lisp

(in-package #:eshop)

(log:config :remove 1)
(log:config :remove 1)

(log:config (log:category '(eshop)) ; Configure appenders for CURRENT PACKAGE
            :sane                   ; Configure default appenders
            :this-console           ; Always print to the server console too
            :nopretty               ; Avoid splitting log lies
            :time                   ; Include time in log messages
            :nofile                 ; Don't include source file name
            :debug                  ; Activate debug level for console logs
            )

(log:config :warn                                                                         ; Log only warnings and errors to file
            :daily (merge-pathnames "eshop.log" (config.get-option :paths :path-to-logs)) ; requests log
            :backup nil                                                                   ; just one plain file, without rolling
            :nopretty
            :time
            :nofile
            )

(defun log-to-this-console ()
  (log:config (log:category '(eshop)) :tricky))
