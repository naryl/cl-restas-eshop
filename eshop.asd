(defsystem eshop
  :depends-on (#:restas #:hunchentoot #:closure-template
               #:cl-csv #:cl-mime #:data-sift #:cl-heap #:st-json
               #:mongo-cl-driver #:mongo-cl-driver.usocket
               #:sb-concurrency #:closer-mop #:bordeaux-threads
               #:function-cache
               #:metric #:log4cl #:drakma
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
               (:module "base"
                        :pathname "."
                        :serial t
                        :components ((:file "packages")
                                     (:file "config")
                                     (:file "log")
                                     (:file "search-tips")
                                     (:file "images") ;; imagemagic
                                     (:file "time")
                                     (:file "eshop-config")
                                     (:file "errors")
                                     (:file "servo")))
               (:module "store"
                        :pathname "."
                        :serial t
                        :components ((:file "process")
                                     (:file "mop-odm")
                                     (:file "storage")
                                     (:file "sessions")
                                     (:file "users")
                                     (:file "slots")
                                     (:file "backup")
                                     (:file "class-core")
                                     (:file "yml")
                                     (:file "classes")))
               (:module "pages"
                        :pathname "."
                        :serial t
                        :components ((:file "main-page")
                                     (:file "catalog")
                                     (:file "newcart")
                                     (:file "gateway")
                                     (:file "search")
                                     (:file "render")
                                     (:file "articles")
                                     (:file "static-pages" :depends-on ("articles"))
                                     (:file "cart")
                                     (:file "oneclickcart")
                                     (:file "filters")
                                     (:file "routes")))
               (:module "misc"
                        :pathname "."
                        :serial t
                        :components ((:file "xls")  ;;необходима xls2csv | sudo apt-get install catdoc
                                     (:file "delivery-price")
                                     (:file "sklonenie")
                                     (:file "sitemap")
                                     (:file "rename")
                                     (:file "prerender")
                                     (:file "marketing-filters")
                                     (:file "admin")
                                     (:file "sendmail")
                                     (:file "email")
                                     (:file "groupd")
                                     (:file "cartrige")
                                     (:file "report")
                                     (:file "black-list")
                                     (:module "cl-cron"
                                              :components ((:file "packages")
                                                           (:file "cl-cron")))
                                     (:file "cron")
                                     (:file "debug")
                                     (:file "bonus-activate")))))
