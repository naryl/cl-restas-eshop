;;;; log.lisp

(in-package #:eshop)

(log:config :sane ; Configure appenders
            :this-console ; Always print to the server console too
            :nopretty ; Avoid splitting log lies
            :time ; Include time in log messages
            :nofile ; Don't include source file name
            )
