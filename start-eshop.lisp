;; загрузка модулей и файлов
(defparameter *path-to-libs* (sb-unix::posix-getenv "LIBS_PATH"))
(defparameter *path-to-eshop* (sb-unix::posix-getenv "ESHOP_PATH"))
(defparameter *path-to-config* (sb-unix::posix-getenv "CONFIG_PATH"))

;; регестрация путей для asdf
(load (merge-pathnames "load.lisp" *path-to-eshop*))
(load.register-libs)

;; load swank libs
(asdf:load-system :swank)

;; для того чтобы загружался esrap
(load (merge-pathnames "slime-archimag/contrib/swank-indentation.lisp" *path-to-libs*))

;; load eshop
(push *path-to-eshop* asdf:*central-registry*)
(asdf:load-system :eshop)

;; swank server start
(print swank::*application-hints-tables*)
(setq swank:*use-dedicated-output-stream* nil)
(swank:create-server :coding-system "utf-8-unix"
										 :dont-close t
										 :port (eshop:config.get-option "START_OPTIONS" "swank-port"))

;; alternative order numbering for developers server
(if (and (not (eshop:config.get-option "START_OPTIONS" "release"))
				 (eshop:config.get-option "START_OPTIONS" "dbg-on"))
		(progn
			;; нумерация заказов
			(setf eshop::*order-id* 1)
			(setf eshop::*path-order-id-file* "wolfor-order-id.txt")
			;; адрес для карты сайта
			;;(setf eshop:*path-sitemap* "wolfor-sitemap.xml")
			;; Список email для рассылки писем от ошибках выгрузки 1с
			(setf eshop::*conf.emails.gateway.warn* (list "wolforus@gmail.com"))
			;; Список email для отправки заказов
			(setf eshop::*conf.emails.cart* (list "wolforus@gmail.com"
																						"slamly@gmail.com"))))

(if (eshop:config.get-option "START_OPTIONS" "dbg-on")
		(restas:debug-mode-on)
		(restas:debug-mode-off))
(setf hunchentoot:*catch-errors-p* (eshop:config.get-option "START_OPTIONS" "catch-errors"))
;; запуск Restas
(restas:start '#:eshop :port (eshop:config.get-option "START_OPTIONS" "server-port"))

(let ((*package* (find-package :eshop)))
	;;; content
  (when (eshop:config.get-option "START_OPTIONS" "load-storage")
    (eshop:sklonenie.restore)
		(eshop:new-classes.unserialize-all)
		(eshop:gateway.restore-history))
	(when (eshop:config.get-option "START_OPTIONS" "load-xls")
		(eshop:dtd)
		(eshop::groupd.restore)
		(eshop::cartrige.restore)
		(eshop::groupd.holiday.restore))
	(when (eshop:config.get-option "START_OPTIONS" "load-content")
		(eshop:static-pages.restore)
		(eshop:articles.restore)
		(eshop:main-page.restore))
	(when (eshop:config.get-option "START_OPTIONS" "run-cron-jobs")
		;; making timer for backups
		(cl-cron:make-cron-job #'eshop::backup.serialize-all :minute 0 :hour 17)
		(cl-cron:start-cron))
  ;;; business logic
  (when (eshop:config.get-option "START_OPTIONS" "make-marketing-filters")
    (eshop::report.create-marketing-filters)
    (eshop::report.set-salefilter)))

(print "ESHOP load finished")
