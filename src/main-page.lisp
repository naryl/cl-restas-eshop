;;;; main-page.lisp

(in-package #:eshop)

;;обновление главной страницы
(defun main-page-update ()
  (servo.compile-soy "main-page.soy"))


;; Имя берется из объявления
;; цена из хранилища товаров
(defun main-page-view-product (key storage)
  (let* ((dp (gethash key storage))
         (p (getobj (key dp) 'product))
         (price (+ (siteprice p) (delta-price p)))
         (parent (parent p))
         (p-list (list :articul (articul p)
                       :name (name dp)
                       :siteprice (siteprice p)
                       :price price
                       :showsiteprice (get-format-price (siteprice p))
                       :showprice (get-format-price price)
                       :url (articul p)
                       :options (opts dp)
                       :grouplink (aif parent
                                       (key it))
                       :groupname (aif parent
                                       (name it))
                       :pic (car (get-pics (key p)))))
         (button-add (list :buttonaddcart (soy.buttons:add-product-cart p-list))))
    (append p-list button-add)))


;;отображение товаров дня
(defun main-page-products-show (storage num)
  (let ((full-daily-list (main-page-get-active-product-list storage))
        (daily-list))
    ;;для блока дэйли должно быть не менее 6 товаров
    (if (> (length full-daily-list) num)
        ;; если активных товаров хватает для демонстрации на главной
        ;; выбираем 6 случайных товаров с учетом их веса
        (setf daily-list (main-page-get-randoms-from-weight-list full-daily-list num))
        ;; если не хватает
        (progn
          ;; (log:info "WARNING: Main page daily ~a products" (length full-daily-list))
          (setf daily-list (main-page-get-randoms-from-weight-list full-daily-list num))))
    (mapcar #'(lambda (v) (main-page-view-product (car v) storage))
            daily-list)))

;;отображение товаров дня
(defun main-page-show-banner (type storage)
  (let ((banners (main-page-get-active-banners storage type))
        (banner (make-instance 'main-page-product)))
    ;;должен быть хотябы один баннер
    (if (plusp (length banners))
        (progn
          ;; выбираем случайный товаров баннер с учетом их веса
          (setf banner (gethash (caar (main-page-get-randoms-from-weight-list banners 1))
                                storage)))
        ;; (log:info "WARNING: No banner"))
        )
    (if banner
        (list :url (format nil "~a"
                           (servo.edit-get-param (encode-uri (nth 1 (opts banner))) "bannerType" type))
              :src (nth 2 (opts banner))
              :name (name banner)
              :src2 (nth 3 (opts banner))
              :type (banner-type banner))
        (list :url ""
              :src ""))))

;;отображение отзыва
(defun main-page-show-lastreview (storage)
  (let ((items (main-page-get-active-views storage))
        (item (make-instance 'main-page-product)))
    ;;должен быть хотябы один баннер
    (if (plusp (length items))
        ;; выбираем случайный товаров баннер с учетом их веса
        (setf item (gethash (caar (main-page-get-randoms-from-weight-list items 1))
                            storage))
        ;; (log:info "WARNING: No banner"))
        )
    (list :name (key item)
          :review (name item)
          :ico (nth 0 (opts item))
          :city (nth 1 (opts item)))))


;;отображение главной страницы
(defun main-page-show ()
  (let ((center-banner (main-page-show-banner "center" (banner *main-page.storage*))))
    (default-page
        (soy.index:content
         (list :menu (render.menu)
               :dayly  (soy.main-page:daily (list :items (main-page-products-show (daily *main-page.storage*) 6)))
               :banner (if (string= "swf" (getf center-banner :type))
                           (soy.main-page:banner center-banner)
                           (soy.main-page:banner-center-png center-banner))
               :olist (soy.main-page:olist)
               :lastreview (soy.main-page:lastreview (main-page-show-lastreview (review *main-page.storage*)))
               :best (soy.main-page:best (list :items (main-page-products-show (best *main-page.storage*) 12)))
               :hit (soy.main-page:hit (list :items (main-page-products-show (hit *main-page.storage*) 2)))
               :new  (soy.main-page:new (list :items (main-page-products-show (new *main-page.storage*) 6)))
               :post (soy.main-page:post
                      (list :news (articles-view-articles (filters.limit-end (articles.sort (get-articles-by-tags (get-articles-list) "новости")) 3))
                            :akcii (articles-view-articles(filters.limit-end (articles.sort (get-articles-by-tags (get-articles-list) "акции")) 3))
                            :reviews (articles-view-articles(filters.limit-end (articles.sort (get-articles-by-tags (get-articles-list) "обзоры")) 3))))
               :plus (soy.main-page:plus)))
        :KEYWORDS "компьютеры, купить компьютер, компьютерная техника, Петербург, Спб, Питер, Санкт-Петербург, продажа компьютеров, магазин компьютерной техники, магазин компьютеров, интернет магазин компьютеров, интернет магазин компьютерной техники, продажа компьютерной техники, магазин цифровой техники, цифровая техника, Цифры, 320-8080"
        :DESCRIPTION "Купить компьютер и другую технику вы можете в Цифрах. Цифровая техника в Интернет-магазине 320-8080.ru"
        :TITLE "Интернет-магазин: купить компьютер, цифровую технику, комплектующие в Санкт-Петербурге")))

(defclass main-page-storage ()
  ((daily          :initarg :daily       :initform (make-hash-table :test #'equal)     :accessor daily)
   (best           :initarg :best        :initform (make-hash-table :test #'equal)     :accessor best)
   (hit            :initarg :hit         :initform (make-hash-table :test #'equal)     :accessor hit)
   (new            :initarg :new         :initform (make-hash-table :test #'equal)     :accessor new)
   (review         :initarg :review      :initform (make-hash-table :test #'equal)     :accessor review)
   (banner         :initarg :banner      :initform (make-hash-table :test #'equal)     :accessor banner)))

(defparameter *main-page.storage* (make-instance 'main-page-storage))

;; продукт для главной
(defclass main-page-product ()
  ((key            :initarg :key         :initform nil     :accessor key)
   (name           :initarg :name        :initform nil     :accessor name)
   (date-start     :initarg :date-start  :initform nil     :accessor date-start)
   (date-finish    :initarg :date-finish :initform nil     :accessor date-finish)
   (weight         :initarg :weight      :initform 0       :accessor weight)
   (opts           :initarg :opts        :initform nil     :accessor opts)
   (banner-type    :initarg :banner-type :initform nil     :accessor banner-type)))

;;получить список активных продуктов из хэш таблицы
(defun main-page-get-active-product-list (storage)
  (let ((rs))
    (maphash #'(lambda (k v)
                 (when v
                   (let ((p (getobj (key v) 'product)))
                     (when (and p (active p)
                                (< (date-start v) (get-universal-time) (date-finish v)))
                       (push (cons k (weight v)) rs)))))
             storage)
    rs))

;;получить список активных баннеров из хэш таблицы
(defun main-page-get-active-banners (storage place)
  (let ((rs))
    (maphash #'(lambda (k v)
                 (when v
                   (if (and (equal (key v) place)
                            (< (date-start v) (get-universal-time) (date-finish v)))
                       (push (cons k (weight v)) rs))))
             storage)
    rs))

;;получить список активных элементов
(defun main-page-get-active-items (storage)
  (let ((rs))
    (maphash #'(lambda (k v)
                 (when v
                   (if (and (getobj (key v))
                            (< (date-start v) (get-universal-time) (date-finish v)))
                       (push (cons k (weight v)) rs))))
             storage)
    rs))

;;получить список активных элементов
(defun main-page-get-active-views (storage)
  (let ((rs))
    (maphash #'(lambda (k v)
                 (when v
                   (if  (< (date-start v) (get-universal-time) (date-finish v))
                        (push (cons k (weight v)) rs))))
             storage)
    rs))


;;получить позицию элемента в списке с весами
(defun main-page-get-num-in-weight-list (input-list weight)
  (let ((cur-pos 0)
        (cur-weight 0))
    (mapcar #'(lambda (v)
                (if (and (> (+ cur-weight (cdr v)) weight)
                         (<= cur-weight weight))
                    (return-from main-page-get-num-in-weight-list cur-pos)
                    (progn
                      (setf cur-weight (+ cur-weight (cdr v)))
                      (incf cur-pos))))
            input-list)
    cur-pos))

;; выбор нескольких случайных элементов из списка c учетом их веса
;; если количество не указано то возвращается список из одного элемента
;; если количество больше длинны входного списка, то возвращается перемешанный входной список
(defun main-page-get-randoms-from-weight-list (input-list &optional (count 1))
  (let ((result)
        (sum-weight 0)
        (current-list input-list))
    ;;уменьшаем count до длинны списка если надо
    (if (< (length input-list)
           count)
        (setf count (length input-list)))
    (mapcar #'(lambda (v) (setf sum-weight (+ sum-weight (cdr v))))
            input-list)
    (setf result (loop
                    :for n
                    :from 1 to count
                    :collect (let* ((weight-pos 0)
                                    (pos 0)
                                    (element nil))
                               (when (plusp sum-weight)
                                 (setf weight-pos (random sum-weight))
                                 (setf pos (main-page-get-num-in-weight-list current-list weight-pos)))
                               (setf element (nth pos current-list))
                               (setf sum-weight (- sum-weight (cdr element)))
                               (setf current-list (remove-if #'(lambda (v)
                                                                 (equal v element))
                                                             current-list))
                               element)))
    result))

(defun main-page.restore ()
  (let* ((t-storage (make-instance 'main-page-storage)))
    (log:info "Start main-page.restore...")
    (loop
       :for filename
       :in (list "daily.xls"         "best.xls"       "hit.xls"       "new.xls"
                 "banners.xls"       "review.xls")
       :for storage
       :in (list (daily t-storage)   (best t-storage) (hit t-storage) (new t-storage)
                 (banner t-storage)  (review t-storage))
       :do (let ((num 0))
             (xls.restore-from-xls
              (merge-pathnames filename (config.get-option :paths :path-to-main-page))
              #'(lambda (line)
                  (let* ((words (sklonenie-get-words line))
                         (skls (mapcar #'(lambda (w) (string-trim "#\""  w))
                                       words))
                         (key (car skls)))
                    (when key
                      (let ((offer (make-instance 'main-page-product
                                           :key key
                                           :name (nth 1 skls)
                                           :date-start (time.article-decode-date (nth 2 skls))
                                           :date-finish  (time.article-decode-date (nth 3 skls))
                                           :weight (parse-integer (aif (nth 4 skls) it "0"))
                                           :opts (nthcdr 5 skls)
                                           :banner-type (nth 5 skls))))
                        (when (< 0 (weight offer))
                          (incf num)
                          (setf (gethash num storage) offer)))))))))
    (setf *main-page.storage* t-storage)
    (log:info "Finish main-page.restore...")))
