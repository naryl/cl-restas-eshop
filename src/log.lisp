;;;; log.lisp

(in-package #:eshop)

(log:config :sane ; Configure appenders
            :this-console ; Always print to the server console too
            :nopretty ; Avoid splitting log lies
            :time ; Include time in log messages
            :nofile ; Don't include source file name
            )

(defvar *eshop-access-log-lock* (bt:make-lock "eshop-request-log-lock"))

(defun request-log-message (control-string &rest args)
 (tbnl::with-log-stream (stream (merge-pathnames "request.log" (config.get-option :paths :path-to-logs)) *eshop-access-log-lock*)
   (apply #'format stream control-string args)))
