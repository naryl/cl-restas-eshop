(defsystem eshop
  :depends-on (#:restas #:hunchentoot #:closure-template
               #:cl-csv #:cl-mime #:data-sift #:cl-heap #:st-json
               #:mongo-cl-driver #:mongo-cl-driver.usocket
               #:log4cl #:drakma
               #:anaphora #:alexandria #:parse-float)
  :pathname "src"
  :serial t
  :components ((:module "patches"
                        :components
                        ((:module "closure-templates"
                                  :components
                                  ((:file "common-lisp-backend")))
                         (:module "cl-mime"
                                  :components ((:file "encoding")
                                               (:file "headers")))
                         (:module "st-json"
                                  :components ((:file "st-json")))))
               (:file "packages")
               (:file "config")
               (:file "log")
               (:file "search-tips")
               (:file "images") ;; imagemagic
               (:file "time")
               (:file "eshop-config")
               (:file "errors")
               (:file "servo")
               (:file "routes")
               (:file "render")
               (:file "cart")
               (:file "xls")  ;;необходима xls2csv | sudo apt-get install catdoc
               (:file "delivery-price")
               (:file "yml")
               (:file "articles")
               (:file "sklonenie")
               (:file "newcart")
               (:file "sitemap")
               (:file "rename")
               (:file "catalog")
               (:file "prerender")
               (:file "storage")
               (:file "sessions")
               (:file "slots")
               (:file "backup")
               (:file "class-core")
               (:file "classes")
               (:file "main-page")
               (:file "filters")
               (:file "marketing-filters")
               (:file "oneclickcart")
               (:file "static-pages")
               (:file "search")
               (:file "admin")
               (:file "gateway")
               (:file "sendmail")
               (:file "email")
               (:file "groupd")
               (:file "cartrige")
               (:file "report")
               (:file "black-list")
               (:module "cl-cron"
                        :components ((:file "packages")
                                     (:file "cl-cron"))
                        :depends-on ("classes"))
               (:file "cron")
               (:file "debug")
               (:file "bonus-activate")))
