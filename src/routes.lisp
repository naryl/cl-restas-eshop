
;;;; routes.lisp

(in-package #:eshop)


(defmethod route-symbol ((route routes:proxy-route))
  (route-symbol (routes:proxy-route-target route)))

(defmethod route-symbol ((route routes:route))
  (restas:route-symbol route))


;;;; check access level by user role

(defclass protected-route (routes:proxy-route)
  ((roles :initform nil
          :initarg :roles)))

(defun @protected (&rest roles)
  #'(lambda (route)
      (make-instance 'protected-route :target route :roles roles)))

(defmethod restas:process-route :before ((route protected-route) bindings)
  (let ((current-user (current-user)))
    (unless (and current-user
                 (some #'(lambda (role)
                           (find role (user-roles current-user) :test 'equal))
                       (slot-value route 'roles)))
      (hunchentoot:redirect "/"
                            :code hunchentoot:+http-moved-permanently+))))

;;;; no cache (use hunchentoot no-cache instead if restas:@no-cache)

(defclass no-cache-route (routes:proxy-route) ())

(defun @no-cache (route)
  (make-instance 'no-cache-route :target route))

(defmethod restas:process-route :before ((route no-cache-route) bindings)
  (hunchentoot:no-cache))

;;;; Request tracking decoration

(defclass timer-route (routes:proxy-route) ())

(defun @timer (route)
  (make-instance 'timer-route :target route))

(defmethod restas:process-route :around ((route timer-route) bindings)
  "log timing and additional data for current route processing and
   update current thread information"
  (metric:count "requests")
  (metric:count (if (bot-request)
                    "bots"
                    "users"))
  (let ((*current-route-symbol* (route-symbol route)))
    (metric:time ((concatenate 'string "process-route." (route-name)))
      (call-next-method))))
;;;; Session decoration

(defclass session-route (routes:proxy-route) ())

(defun @session (route)
  (make-instance 'session-route :target route))

(defmethod restas:process-route :around ((route session-route) bindings)
  "Ensure session for this route"
  ;; set closure template injected data with current session data
  (let ((closure-template:*injected-data* closure-template:*injected-data*))
    (setf (getf closure-template:*injected-data* :session) (start-session)
          (getf closure-template:*injected-data* :user) (current-user)
          (getf closure-template:*injected-data* :currenturl) (restas:request-full-uri))
    (call-next-method)))

(defvar *current-route-symbol* nil)

(defcached route-name (&optional (route *current-route-symbol*))
  (string-downcase (symbol-name route)))

;; static content
;; Роуты до статиx файлов
;; Дублирует функционал nginx для развертывания на localhost

(restas:define-route request-static-route-img ("/img/*")
  (let ((full-uri (format nil "~A" (restas:request-full-uri))))
    (hunchentoot:redirect (concatenate 'string "http://320-8080.ru"
                                       (subseq full-uri (search "/img/" full-uri)))
                          :code hunchentoot:+http-moved-permanently+)))

(restas:define-route request-static-route-pic ("/pic/*")
  (let* ((full-uri (format nil "~A" (restas:request-full-uri))))
    (hunchentoot:redirect (concatenate 'string "http://320-8080.ru"
                                       (subseq full-uri (search "/pic/" full-uri)))
                          :code hunchentoot:+http-moved-permanently+)))

(restas:define-route request-static-route-css ("/css/*")
  (let ((full-uri (format nil "~A" (restas:request-full-uri))))
    (merge-pathnames (concatenate 'string
                                  "htimgs/"
                                  (when (config.get-option :start-options :dbg-on) "dev/")
                                  (subseq full-uri (search "/css/" full-uri)))
                     (config.get-option :paths :path-to-dropbox))))

(restas:define-route request-static-route-js ("/js/*")
  (let ((full-uri (format nil "~A" (restas:request-full-uri))))
    (merge-pathnames (concatenate 'string
                                  "htimgs/"
                                  (when (config.get-option :start-options :dbg-on) "dev/")
                                  (subseq full-uri (search "/js/" full-uri)))
                     (config.get-option :paths :path-to-dropbox))))

(restas:define-route request-route-static-favicon ("/favicon.ico")
  (merge-pathnames "htimgs/img/favicon.ico" (config.get-option :paths :path-to-dropbox)))

(restas:define-route request-route-static-robots ("/robots.txt")
  (merge-pathnames "robots.txt" (config.get-option :critical :path-to-conf)))

(restas:define-route request-route-static-yml ("/yml.xml")
  (merge-pathnames "yml.xml" (config.get-option :critical :path-to-conf)))

(restas:define-route request-route-static-sitemap ("/sitemap.xml")
  (merge-pathnames "sitemap.xml" (config.get-option :critical :path-to-conf)))

(restas:define-route request-route-static-sitemap-index ("/sitemap-index.xml")
  (merge-pathnames "sitemap-index.xml" (config.get-option :critical :path-to-conf)))

(restas:define-route request-route-static-sitemap1 ("/sitemap1.xml")
  (merge-pathnames "sitemap1.xml" (config.get-option :critical :path-to-conf)))

(restas:define-route request-route-static-sitemap2 ("/sitemap2.xml")
  (merge-pathnames "sitemap2.xml" (config.get-option :critical :path-to-conf)))

(defvar *search-tips* (make-instance 'search-tips:search-tips))

(restas:define-route request-suggestions ("/api/suggestions" :method :get)
  (:decorators '@timer)
  (let ((prefix (tbnl:get-parameter "prefix"))
        (k (aif (tbnl:get-parameter "k") (parse-integer it) 10)))
    (if (or (null prefix) (null k) (not (typep k 'integer)))
        "Incorrect parameters"
        (when (plusp (length (search-tips:tips *search-tips*)))
          (htmlize (format nil "~{~A~%~}"
                           (mapcar #'(lambda (tip)
                                       (format nil "~A ~A" (search-tips:weight tip) (search-tips:tip tip)))
                                   (reverse (search-tips:max-k-tips-by-prefix *search-tips* prefix k)))))))))

;; end static content
;; FILTER

(defun test-route-filter ()
  ;; marketing-filters-test
  (let* ((request-list (request-list))
         (group-key (cadr request-list))
         (filter-key (caddr request-list))
         (group (getobj group-key 'group))
         (filter (getobj filter-key 'filter)))
    (and group
         filter
         (equal group (parent filter)))))

(defun route-filter (filter)
  (getobj filter 'filter))

(restas:define-route filter-route ("/:key/:filter")
  (:requirement 'test-route-filter)
  (:decorators '@timer '@session)
  (:render-method (make-instance 'eshop-render))
  (declare (ignore key))
  (route-filter filter))

(restas:define-route filter/-route ("/:key/:filter/")
  (hunchentoot:redirect (format nil "/~{~A~^/~}" (list key filter))
                        :code hunchentoot:+http-moved-permanently+))

;; STORAGE OBJECT

(defun vendor-transform-from-alias (alias)
  (aif (getobj alias 'vendor)
       (name it)
       alias))

(defun test-route-storage-object ()
  (let* ((key (cadr (request-list)))
         (obj (getobj key)))
    (if (gethash key static-pages.*storage*)
        t
        (when (and obj
                   (or (groupp obj)
                       (productp obj)))
          (aif (and (groupp obj)
                    (getf (request-get-plist) :vendor))
               (let ((vendor (vendor-transform-from-alias (string-downcase it))))
                 (some #'(lambda (p)
                           (vendor-filter-controller p vendor))
                       (storage.get-recursive-products obj)))
               t)))))

(defun route-storage-object (key)
  (aif (and key (getobj key 'product))
       it
       (aif (and key (getobj key 'group))
            it
            ;; else: static pages
            (gethash key static-pages.*storage*))))

(restas:define-route storage-object-route  ("/:key")
  (:requirement 'test-route-storage-object)
  (:decorators '@timer '@session)
  (:render-method (make-instance 'eshop-render))
  (route-storage-object key))

(restas:define-route storage-object/-route ("/:key/")
  (hunchentoot:redirect (concatenate 'string "/" key)
                        :code hunchentoot:+http-moved-permanently+))

;; MAIN
(defun test-get-parameters ()
  t) ;;(null (request-get-plist)))

(restas:define-route main-route ("/")
  (:requirement 'test-get-parameters)
  (:decorators '@timer '@session)
  (main-page-show))

;; CATALOG

(restas:define-route catalog-page-route ("/catalog")
  (:decorators '@timer '@session)
  (default-page (catalog.catalog-entity)
      :keywords "Купить компьютер и другую технику вы можете в Цифрах. Цифровая техника в Интернет-магазине 320-8080.ru"
      :description "каталог, компьютеры, купить компьютер, компьютерная техника, Петербург, Спб, Питер, Санкт-Петербург, продажа компьютеров, магазин компьютерной техники, магазин компьютеров, интернет магазин компьютеров, интернет магазин компьютерной техники, продажа компьютерной техники, магазин цифровой техники, цифровая техника, Цифры, 320-8080"
      :title "Каталог интернет-магазина: купить компьютер, цифровую технику, комплектующие в Санкт-Петербурге"))

(restas:define-route sitemap-page-route ("/sitemap")
  (:decorators '@timer '@session)
  (default-page (catalog.sitemap-page)
      :keywords "Купить компьютер и другую технику вы можете в Цифрах. Цифровая техника в Интернет-магазине 320-8080.ru"
      :description "каталог, компьютеры, купить компьютер, компьютерная техника, Петербург, Спб, Питер, Санкт-Петербург, продажа компьютеров, магазин компьютерной техники, магазин компьютеров, интернет магазин компьютеров, интернет магазин компьютерной техники, продажа компьютерной техники, магазин цифровой техники, цифровая техника, Цифры, 320-8080"
      :title "Каталог интернет-магазина: купить компьютер, цифровую технику, комплектующие в Санкт-Петербурге"))


;; CART & CHECKOUTS & THANKS

(restas:define-route cart-route ("/cart")
  (:decorators '@timer '@session)
  (cart-page))

(restas:define-route checkout-route ("/checkout")
  (:decorators '@timer '@session)
  (newcart-show))

(restas:define-route checkout-post-route ("/checkout" :method :post)
  (:decorators '@timer '@session)
  (newcart-show))

(restas:define-route checkout0-route ("/checkout0")
  (:decorators '@timer '@session)
  (newcart-show))

(restas:define-route checkout1-route ("/checkout1")
  (:decorators '@timer '@session)
  (newcart-show))

(restas:define-route checkout2-route ("/checkout2")
  (:decorators '@timer '@session)
  (newcart-show))

(restas:define-route checkout3-route ("/checkout3")
  (:decorators '@timer '@session)
  (newcart-show))

(restas:define-route thanks-route ("/thanks")
  (:decorators '@timer '@session '@no-cache)
  (thanks-page))


;; GATEWAY

(restas:define-route gateway/post-route ("/gateway" :method :post)
  (:decorators '@timer)
  (gateway-page))

(restas:define-route gateway-ekk/post-route ("/gateway-ekk" :method :post)
  (:decorators '@timer)
  (gateway-ekk))

;; SEARCH

(restas:define-route search-route ("/search")
  (:decorators '@timer '@session)
  (search-page))

;; YML

(restas:define-route yml-route ("/yml")
  (:decorators '@timer)
  (yml-page))

(restas:define-route yml/-route ("/yml/")
  (hunchentoot:redirect "/yml"
                        :code hunchentoot:+http-moved-permanently+))

;; ARTICLES
;;TODO возможно проверять входные тэги
(defun test-article-get-parameters ()
  t)

;;проверяем есть ли такая статья
(defun test-route-article-object ()
  (not (null (gethash (caddr (request-list)) *storage-articles*))))

;;архив матерьялов
(restas:define-route article-route ("/articles")
  (:requirement 'test-article-get-parameters)
  (:decorators '@timer '@session)
  (articles-page (request-get-plist)))

;;список статей
(restas:define-route article-papers-route ("/articles/papers")
  (:requirement 'test-article-get-parameters)
  (:decorators '@timer '@session)
  (let ((request-get-plist (request-get-plist)))
    (if (null (getf request-get-plist :tags))
        (setf (getf request-get-plist :tags) "Статьи"))
    (articles-page request-get-plist)))

;;список акции
(restas:define-route article-akcii-route ("/articles/akcii")
  (:requirement 'test-article-get-parameters)
  (:decorators '@timer '@session)
  (let ((request-get-plist (request-get-plist)))
    (if (null (getf request-get-plist :tags))
        (setf (getf request-get-plist :tags) "текущии акции"))
    (articles-page request-get-plist)))

;;список новостей
(restas:define-route article-news-route ("/articles/news")
  (:requirement 'test-article-get-parameters)
  (:decorators '@timer '@session)
  (let ((request-get-plist (request-get-plist)))
    (if (null (getf request-get-plist :tags))
        (setf (getf request-get-plist :tags) "Новости"))
    (articles-page request-get-plist)))

;;список обзоры
(restas:define-route article-review-route ("/articles/reviews")
  (:requirement 'test-article-get-parameters)
  (:decorators '@timer '@session)
  (let ((request-get-plist (request-get-plist)))
    (if (null (getf request-get-plist :tags))
        (setf (getf request-get-plist :tags) "Обзоры"))
    (articles-page request-get-plist)))

;;конкретная статья
(restas:define-route article-key-route ("/articles/:key")
  (:requirement 'test-route-article-object)
  (:decorators '@timer '@session)
  (:render-method (make-instance 'eshop-render))
  (gethash (caddr (request-list)) *storage-articles*))

;; 404

;;необходимо отдавать 404 ошибку для несуществеющих страниц

(restas:define-route not-found-route ("*any")
  (:decorators '@timer '@session)
  ;; (log:warn "error 404: ~A" any)
  (restas:abort-route-handler
   (babel:string-to-octets
    (default-page
        (soy.er404:content
         (list :menu (render.menu)
               :dayproducts (main-page-products-show (daily *main-page.storage*) 4)
               :olist (soy.main-page:olist)
               :lastreview (soy.main-page:lastreview (main-page-show-lastreview (review *main-page.storage*)))
               :bestpriceproducts (main-page-products-show (best *main-page.storage*) 4)
               :hit (soy.main-page:hit (list :items (main-page-products-show (hit *main-page.storage*) 2)))
               :newproducts (main-page-products-show (new *main-page.storage*) 4)
               :post (soy.main-page:post
                      (list :news (articles-view-articles (filters.limit-end (articles.sort (get-articles-by-tags (get-articles-list) "новости")) 3))
                            :akcii (articles-view-articles(filters.limit-end (articles.sort (get-articles-by-tags (get-articles-list) "акции")) 3))
                            :reviews (articles-view-articles(filters.limit-end (articles.sort (get-articles-by-tags (get-articles-list) "обзоры")) 3))))
               :plus (soy.main-page:plus)))
        :keywords "Купить компьютер и другую технику вы можете в Цифрах. Цифровая техника в Интернет-магазине 320-8080.ru"
        :description "каталог, компьютеры, купить компьютер, компьютерная техника, Петербург, Спб, Питер, Санкт-Петербург, продажа компьютеров, магазин компьютерной техники, магазин компьютеров, интернет магазин компьютеров, интернет магазин компьютерной техники, продажа компьютерной техники, магазин цифровой техники, цифровая техника, Цифры, 320-8080"
        :title "Каталог интернет-магазина: купить компьютер, цифровую технику, комплектующие в Санкт-Петербурге")
    :encoding :utf-8)
   :return-code hunchentoot:+http-not-found+
   :content-type "text/html"))

(restas:define-route request-route ("/request")
  (:decorators '@timer '@session)
  (oneclickcart.make-common-order (request-get-plist)))

(restas:define-route compare-route ("/compare")
  (:decorators '@timer '@session)
  (log:debug "IP:~A" (tbnl:real-remote-addr))
     (soy.compare:compare-page
        (list :keywords "" ;;keywords
                    :description "" ;;description
                    :title ""
                    :header (soy.header:header (append (list :cart (soy.index:cart))
                                                       (main-page-show-banner "line" (banner *main-page.storage*))))
                    :footer (soy.footer:footer))))

;;;; Stuff

(defmethod print-object ((route restas::route) stream)
  (print-unreadable-object (route stream :type t :identity t)
    (format stream "~A" (restas::route-symbol route))))
