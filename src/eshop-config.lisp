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

(defun init-closure-user-functions ()
  (closure-template::add-user-function "date" #'(lambda (timestamp) (render-time +date+ timestamp)))
  (closure-template::add-user-function "productpics" #'get-pics)
  (closure-template::add-user-function "parent" #'(lambda (obj) (parent obj)))
  (closure-template::add-user-function "gaCustomVars" #'(lambda (obj) (ga-custom-vars obj))))

(log:info "Compiling all templates")
(init-closure-user-functions)
(compile-templates)
(log:info "Compiling all templates finish")
