;;;; eshop-config.lisp

(in-package #:eshop)

(config.parse-config)


(defun get-all-template-paths ()
  (directory (merge-pathnames (config.get-option :paths :path-to-templates) "*.soy")))

(defun compile-templates ()
  (mapcar #'(lambda (template)
                (log:info template)
                (closure-template:compile-template :common-lisp-backend template))
          (get-all-template-paths)))

(log:info "Compiling all templates")
(compile-templates)
(log:info "Compiling all templates finish")
