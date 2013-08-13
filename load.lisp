(load "~/app/sbcl-1.1.10/contrib/asdf/asdf.lisp")
(require 'asdf)

(defvar *subdir-search-registry* '(#P"~/eshop/aux-libs/")
  "List of directories to search subdirectories within.")
(defvar *subdir-search-wildcard* :wild-inferiors
  "Value of :wild means search only one level of subdirectories; value of :wild-inferiors means search all levels of subdirectories (I don't advise using this in big directories!)")
(defun sysdef-subdir-search (system)
  (let ((latter-path (make-pathname :name (asdf:coerce-name system)
                                    :directory (list :relative
                                                     *subdir-search-wildcard*)
                                    :type "asd"
                                    :version :newest
                                    :case :local)))
    (dolist (d *subdir-search-registry*)
      (let* ((wild-path (merge-pathnames latter-path d))
             (files (directory wild-path)))
        (when files
          (return (first files)))))))

(defun load.register-libs ()
  (progn
    (pushnew 'sysdef-subdir-search asdf:*system-definition-search-functions*)
    (mapcar #'(lambda (lib-name)
                (push (truename (merge-pathnames lib-name *path-to-libs*)) asdf:*central-registry*))
            (list
             "slime" ;; SWANK
             ;; WEB Server
             "hunchentoot" ;; HUNCH restas
             "cl-fad" ;; hunch (test ok)
             "cl-ppcre" ;; hunch (test ok)
             "flexi-streams" ;; hunch cl-ppcre (test ok)
             "trivial-gray-streams" ;; flexi-streams
             "chunga" ;; hunch
             "md5" ;; hunch
             "cl-base64" ;;hunch
             "rfc2388" ;; hunch
             "trivial-backtrace" ;; hunch
             "usocket" ;; hunch
             "bordeaux-threads" ;; hunch restas
             "alexandria" ;; bordeaux-threads restas
             "cl-plus-ssl" ;; hunch
             "trivial-garbage" ;; cl-plus-ssl
             "cffi" ;; cl-plus-ssl
             "babel"   ;; cffi
             "trivial-features" ;; babel
             ;; WEB FRAMEWORK RESTAS
             "restas" ;; RESTAS
             "cl-routes" ;; restas
             "split-sequence" ;; cl-routes
             "iterate" ;; cl-routes
             "data-sift" ;; restas
             "puri-unicode" ;; data-sift
             "parse-number" ;; data-sift
					   "closer-mop" ;;restas
             ;; CLOP (ESHOP)
             "cl-json" ;; eshop JSON сериализатор
             "cl-closure-template" ;; eshop шаблонизатор
             "parenscript" ;; closure-template
             "named-readtables" ;; parenscript
             "anaphora" ;; parenscript | macro collection from Hell http://www.cliki.net/Anaphora
             "esrap" ;; closure-template
             "arnesi" ;; eshop parse-float
             "log5" ;; eshop
             "string-case" ;; eshop
             "cl-csv" ;; eshop
             "cl-interpol" ;; cl-csv
             "cl-unicode" ;; cl-csv
             "cl-mime" ;; eshop
             "cl-qprint" ;; cl-mime
             "cl-heap" ;; heap and priority queue

             ;; WEB Server test
             ;; "cl-who" ;; hunch-test
             ;; "drakma" ;; hunch-test http клиент для тестов
             ;; "puri-unicode" ;; drakma
             ;; "Eos" ;; esrap test


           ;; ;;mongoDB
           ;; "cl-mongo"
           ;; "lisp-unit" ;; mongo
           ;; "documentation-template" ;; mongo
           ;; "uuid" ;; mongo
           ;; "trivial-utf-8" ;; mongo
           ;; "ironclad" ;; mongo
           ;; "nibbles" ;; mongo
           ;; ;; "js" ;; parenscript-test
           ;; ;; "parse-js" ;; -> js
           ;; ;; "Eos" ;; -> js
           ;; "rt-20040621" ;; usocket-test
           ;;               ;; These extensions have been incorporated into sbcl's sb-rt package
  ))))
