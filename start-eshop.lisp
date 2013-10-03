(in-package #:cl-user)

;; загрузка модулей и файлов
(defparameter *path-to-libs* (sb-unix::posix-getenv "LIBS_PATH"))
(defparameter *path-to-eshop* (sb-unix::posix-getenv "ESHOP_PATH"))
(defparameter *path-to-config* (sb-unix::posix-getenv "CONFIG_PATH"))
(defparameter *debug* (sb-unix::posix-getenv "DEBUG"))
(defparameter *swank-port* (parse-integer (sb-unix::posix-getenv "SWANK_PORT")))


;; регестрация путей для asdf
(load (merge-pathnames "load.lisp" *path-to-eshop*))
(load.register-libs *path-to-libs*)


(when *debug*
    (restrict-compiler-policy 'debug 1)
    (restrict-compiler-policy 'safety 1)
    (restrict-compiler-policy 'speed 3)
    (restrict-compiler-policy 'space 3)
    (asdf:load-system :mongo-cl-driver :force t)
    (push :debug *features*)
    (restrict-compiler-policy 'debug 3)
    (restrict-compiler-policy 'safety 3)
    (restrict-compiler-policy 'speed 1)
    (restrict-compiler-policy 'space 1))

;; load swank libs
(asdf:load-system :swank)
(swank:create-server :dont-close t
                     :port *swank-port*)

;; load eshop
(push *path-to-eshop* asdf:*central-registry*)
(asdf:load-system :eshop)

;; Metrics
(metric:configure (eshop:config.get-option :critical :graphite-prefix)
                  :host (eshop:config.get-option :critical :graphite-host)
                  :port (eshop:config.get-option :critical :graphite-port)
                  :interval 60)
(setf metric:*error-handler* #'(lambda (e) (log:error "Error submitting metrics: ~S" e)))

;; json serializer config
(setf st-json:*decode-objects-as* :alist)
(setf st-json:*read-null-as* nil)
(setf st-json:*map-keys* #'(lambda (key) (intern
                                          (st-json:camel-dash (string key))
                                          :keyword)))

(if (eshop:config.get-option :start-options :dbg-on)
    (restas:debug-mode-on)
    (restas:debug-mode-off))
(setf hunchentoot:*catch-errors-p* (eshop:config.get-option :start-options :catch-errors))

(eshop.odm:connect "zifry")

(let ((*package* (find-package :eshop)))
    ;;; content
  (when (eshop:config.get-option :start-options :load-storage)
    (eshop:sklonenie.restore)
    (eshop:delivery-price.restore)
    (eshop:class-core.unserialize-all :filesystem)
    (eshop:gateway.load))
  (when (eshop:config.get-option :start-options :load-xls)
    (eshop:xls.update-options-from-xls)
    (eshop:cartrige.restore))
  (when (eshop:config.get-option :start-options :load-content)
    (eshop:static-pages.restore)
    (eshop:articles.restore)
    (eshop:main-page.restore))
  (when (eshop:config.get-option :start-options :run-cron-jobs)
    ;; making timer for backups
    (cl-cron:make-cron-job #'eshop::backup.serialize-all :minute 0 :hour 17)
    (cl-cron:start-cron))
  ;;; business logic
  (eshop::filters.create-standard-filters)
  (when (eshop:config.get-option :start-options :make-marketing-filters)
    (eshop::groupd.restore)
    (eshop::groupd.holiday.restore)
    (eshop::marketing-filters.create-all-filters)))

;; csv parameters
(setf cl-csv:*newline* (string #\Newline)
      cl-csv:*separator* #\;)

(log:info "ESHOP load finished. Time : ~A" (eshop::time.msecs-to-hms (get-internal-real-time)))
(log:info "Server info: ~A" (with-output-to-string (*standard-output*) (room)))

;; запуск Restas
(restas:start '#:eshop :port (eshop:config.get-option :start-options :server-port))
