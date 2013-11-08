
(defpackage eshop.parse-ym
  (:shadowing-import-from :let+ :let+)
  (:use :cl
        :let+
        :eshop.utils
        :alexandria
        :anaphora
        :cl-html-parse
        :cl-html-query))

(in-package eshop.parse-ym)

(defvar *data* (make-hash-table))
(defvar *details* (make-hash-table))

(defun get-all-img-data ()
  (setf *data* (make-hash-table))
  (let ((group (eshop::getobj "watch"))
        (path (merge-pathnames "market-images/"
                               (eshop::config.get-option :paths :path-to-logs))))
    (dolist (product (slot-value group 'eshop::products))
      (let+ ((articul (slot-value product 'eshop::articul))
             (name (slot-value product 'eshop::name-seo))
             ((page productid) (get-product-page name))
             (path (merge-pathnames (format nil "~A/~A.jpg" articul name)
                                    path))
             (img-url (get-product-image-url page name)))
        (setf (gethash articul *data*)
              (list name (when img-url productid) img-url))
        (when img-url
          (ensure-directories-exist path)
          (write-byte-vector-into-file (drakma:http-request img-url
                                                            :proxy '("localhost" 8123))
                                       path)))
      (sleep 1))
    (eshop::rename-convert-all :from path)))

(defun details-to-csv (filename)
  (with-open-file (out filename :direction :output :if-exists :supersede)
    (cl-csv:write-csv-row (list "Артикул" "Название в ЯМ"
                                "Тип" "Механизм"
                                "Скелетон" "Способ отображения времени"
                                "Цифры" "Источник энергии"
                                "Противоударные" "Водонепроницаемые"
                                "Материал корпуса" "Материал браслета/ремешка"
                                "Вставка" "Стекло"
                                "Хронограф" "Тахиметр"
                                "Точность хода" "Отображение даты"
                                "Спорт-функции" "Подсветка"
                                "Вес" "Размер"
                                "Дополнительные функции" "Дополнительная информация")
                          :stream out)
    (maphash #'(lambda (k v)
                 (flet ((param (name)
                          (getf v name "")))
                   (cl-csv:write-csv-row
                    (list* k
                           (mapcar #'param
                                   (list :ym-name
                                         :type :mechanism
                                         :skeleton :indicator-type
                                         :digits :power
                                         :shockproof :waterproof
                                         :body-material :strap-material
                                         :inlay :glass
                                         :chronograph :tacheometer
                                         :accuracy :date-indication
                                         :sport-features :light
                                         :weight :size
                                         :features :misc)))
                    :stream out)))
             *details*)))

(defun get-all-product-data ()
  (let ((group (eshop::getobj "watch")))
    (dolist (product (slot-value group 'eshop::products))
      (with-simple-restart (continue "Continue")
        (with-slots ((name eshop::name-seo)
                     (articul eshop::articul))
            product
          (cond ((and (zerop (eshop::valid-options product))
                      (not (gethash articul *details*)))
                 (awhen (get-product-data name)
                   (setf (gethash articul *details*)
                         it))
                 (sleep 1))
                (t (princ "."))))))))

(defun get-product-data (product)
  (let+ (((search-html ym-name productid) (get-product-page product)))
    (unless search-html
      (princ "!")
      (return-from get-product-data))
    (let* ((details-url (tag-attr (query search-html ".b-model-friendly__title a")
                                  :href))
           (html (get-parsed-html (format nil "http://market.yandex.ru~A" details-url)))
           (details (remove 2 (cdr (query html ".b-properties tbody"))
                            :key #'length)))
      (princ (if (equal ym-name product)
                 "+"
                 "-"))
      (list* :ym-name ym-name :productid productid
             (loop :for detail :in details
                :for name := (second (xpath detail :th :span))
                :for value := (string-trim '(#\Space #\Newline)
                                           (second (xpath detail :td)))
                :append (loop (with-simple-restart (try-again "Try again")
                                (return (list (product-detail-to-keyword name)
                                              value)))))))))

(defun product-detail-to-keyword (detail-name)
  (switch (detail-name :test #'string=)
    ("Тип" :type)
    ("Механизм" :mechanism)
    ("Скелетон" :skeleton)
    ("Способ отображения времени" :indicator-type)
    ("Цифры" :digits)
    ("Источник энергии" :power)
    ("Противоударные" :shockproof)
    ("Водонепроницаемые" :waterproof)
    ("Материал корпуса" :body-material)
    ("Материал браслета/ремешка" :strap-material)
    ("Вставка" :inlay)
    ("Стекло" :glass)
    ("Хронограф" :chronograph)
    ("Тахиметр" :tacheometer)
    ("Точность хода" :accuracy)
    ("Отображение даты" :date-indication)
    ("Спорт-функции" :sport-features)
    ("Подсветка" :light)
    ("Дополнительная информация" :misc)
    ("Вес" :weight)
    ("Габариты" :size)
    ("Габариты (ШхВ)" :size)
    ("Габариты (ШхВхТ)" :size)
    ("Дополнительные функции" :features)
    (t (error "~S" detail-name))))

(defun get-parsed-html (url)
  (let+ (((:values body _ _ uri)
          (drakma:http-request url :proxy '("localhost" 8123))))
    (values (parse-html body) uri)))

(defun get-product-page (product)
  (let+ (((:values html _ _ uri)
          (drakma:http-request "http://market.yandex.ru/search.xml"
                               :parameters (list (cons "cvredirect" "2")
                                                 (cons "text" product))
                               :proxy '("localhost" 8123)))
         (html (parse-html html))
         (have-modelid (second (multiple-value-list
                                (cl-ppcre:scan-to-strings "modelid=(\\d+)"
                                                          (puri:uri-query uri)))))
         (modelid (when have-modelid (parse-integer (aref have-modelid 0)))))
    (list html (page-product html) modelid)))

(defun page-product (page)
  (tag-attr (query page ".b-model-pictures__big a meta") :content))

(defun get-product-image-url (page product)
  (when (equal product (page-product page))
    (let ((img-span (query page ".b-model-pictures__big")))
      (or (tag-attr (query img-span "a") :href)
          (tag-attr (query img-span "img") :src)))))
