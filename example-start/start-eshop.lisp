;; загрузка модулей и файлов
(defvar *path-to-eshop-home* "cl-restas-eshop")
(defvar *swank-port* 4005)
(defvar *server-port* 8080)

(defparameter *path-to-libs* (sb-unix::posix-getenv "LIBS_PATH"))
(defparameter *path-to-eshop* (sb-unix::posix-getenv "ESHOP_PATH"))
(defparameter *path-to-config* (sb-unix::posix-getenv "CONFIG_PATH"))

;; регестрация путей для asdf
(load (format nil "~a~a" *path-to-eshop* "load.lisp"))
(load.register-libs)


;; load swank libs
(asdf:load-system :swank)

;; для того чтобы загружался esrap
(load (format nil "~a~a" *path-to-libs* "slime-archimag/contrib/swank-indentation.lisp"))

;; load eshop
(asdf:load-system :eshop)

;; parse config
(eshop:config.parse-config *path-to-config*)

;; swank server start
(print swank::*application-hints-tables*)
(setq swank:*use-dedicated-output-stream* nil)
(swank:create-server :coding-system "utf-8-unix"
										 :dont-close t
										 :port (eshop:config.get-option "START_OPTIONS" "swank-port"))

;; нумерация заказов
(setf eshop::*order-id* 1)
(setf eshop:*path-order-id-file* "wolfor-order-id.txt")
;; адрес для карты сайта
(setf eshop:*path-sitemap* "wolfor-sitemap.xml")
;; Список email для рассылки писем от ошибках выгрузки 1с
(setf eshop::*conf.emails.gateway.warn* (list "wolforus@gmail.com"))
;; Список email для отправки заказов
(setf eshop::*conf.emails.cart* (list "wolforus@gmail.com"
                                      "slamly@gmail.com"))

;; запуск Restas
(restas:start '#:eshop :port (eshop:config.get-option "START_OPTIONS" "server-port"))
(if (eshop:config.get-option "START_OPTIONS" "dbg-on")
		(restas:debug-mode-on)
		(restas:debug-mode-off))
(setf hunchentoot:*catch-errors-p* (eshop:config.get-option "START_OPTIONS" "catch-errors"))



;; content

(eshop::restore-skls-from-files)
(eshop::articles.restore)
(eshop::main-page.restore)
(let ((*package* (find-package :eshop)))
  (eshop::new-classes.unserialize-all)
  (eshop::new-classes.unserialize-optgroups
   (pathname (format nil "~atest/topts2.bkp" (user-homedir-pathname)))))
(eshop::static-pages.restore)
(eshop::gateway.restore-history)
(eshop::report.set-salefilter)
(eshop::dtd)
(eshop::groupd.restore)
