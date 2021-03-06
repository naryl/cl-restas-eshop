;;;; report.lisp

;; TODO: move to new package report
(in-package #:eshop)

(defparameter *special-products* (make-hash-table :test #'equal))

(defgeneric special-p (product)
  (:documentation "Checks whether product is in *special-products*"))

(defgeneric report.write-report-with-standard-columns (stream columns-data objects)
  (:documentation "Write report with standart columns"))

(defmethod special-p ((product string))
  (not (null (gethash product *special-products*))))

(defmethod special-p ((product product))
  (when product
    (special-p (key product))))

(defun valid-options (product)
  (declare (product product))
  (let ((num 0))
    (mapcar #'(lambda (optgroup)
                (incf num
                      (count-if #'(lambda (option)
                                    (and option
                                         (valid-string-p (getf option :value))
                                         (not (find (getf option :name)
                                                    (list "Производитель" "Модель" "Гарантия" "Сайт производителя") :test #'equal))))
                                (getf optgroup :options))))
            (remove "Secret" (optgroups product)  ; remove Secret group
                    :test #'equal :key #'(lambda (opt) (getf opt :name))))
    num))

;;;; --------------------------- report mechanism -------------------------------
(defvar report.*standard-report-column-funcs* (make-hash-table :test #'equal)
  "Hash-table for storing standard functions for creating reports by ip.
Key should be (unique) symbol, value - function")

(defun report.register-standard-column (specifier func)
  "Registers function that lately can be used in reports' creating API.
Specifier should be symbol, func should be function.
Result of each function must be formatable (e.g. (format nil \"~A\") must be applicable to it)"
  (declare (symbol specifier) (function func))
  (setf (gethash specifier report.*standard-report-column-funcs*) func))

(defun report.get-standard-column-func (specifier)
  (gethash specifier report.*standard-report-column-funcs*))

(defun report.write-report (stream column-headers column-funcs items)
  "Writes report in .csv format to given stream. Each row is set of column functions
applied to item from given item set.
   NOTE: - stream could be T
         - remove all #\" & #\; for compatibility csv"
  (declare (list column-headers column-funcs items))
  ;; write headers
  (cl-csv:write-csv-row column-headers :stream stream)
  ;; write other rows
  (cl-csv:write-csv
   (mapcar #'(lambda (item)
               (mapcar #'(lambda (func)
                           (funcall func item))
                       column-funcs))
           items)
   :stream stream))


(defmethod report.write-report-with-standard-columns ((stream stream) (columns-data list) (storage-specifier symbol))
  "Writes report using only registered columns functions. Column data should be
list of conses (column-header . column-specifier). Storage-specifier should be symbol, to which
function get-storage is applicable.
 NOTE: stream could be T & NIL"
  (report.write-report-with-standard-columns stream columns-data (collect-storage storage-specifier)))



(defmethod report.write-report-with-standard-columns ((stream stream) (columns-data list) (items list))
  "Writes report using only registered columns functions. Column data should be
list of conses (column-header . column-specifier).
 NOTE: stream could be T & NIL"
  (declare (list columns-data items) (stream stream))
  (loop
     :for (header . specifier) :in columns-data
     :collect header :into headers
     :collect (report.get-standard-column-func specifier) :into funcs
     :finally (report.write-report stream headers funcs items)))

(defun report.%rsc (symbol func)
  (report.register-standard-column symbol func))

;; register standard columns
(defun report.register-standard-columns ()
  (report.%rsc 'item-key #'key)
  ;; product functions
  (report.%rsc 'product-articul #'articul)
  (report.%rsc 'product-price #'price)
  (report.%rsc 'product-siteprice #'siteprice)
  (report.%rsc 'product-name #'name-provider)
  (report.%rsc 'product-name-real #'name-seo)
  (report.%rsc 'product-erp-price #'erp-price)
  (report.%rsc 'product-erp-class #'erp-class)
  (report.%rsc 'product-count-total #'count-total)
  (report.%rsc 'product-count-transit #'count-transit)
  (report.%rsc
   'product-yml-name
   (rcurry #'get-option "Secret" "Yandex"))
  (report.%rsc 'product-yml-show #'yml.yml-show-p)
  (report.%rsc
   'product-seo-text-exists
   #'(lambda (item) (if (valid-string-p (seo-text item)) "есть" "нет")))
  (report.%rsc
   'product-num-pics
   #'(lambda (item) (length (get-pics (key item)))))
  (report.%rsc 'product-valid-options #'valid-options)
  (report.%rsc
   'product-active
   #'(lambda (item) (if (active item) "да" "нет")))
  (report.%rsc
   'product-group
   #'(lambda (item) (when (parent item) (name (parent item)))))
  (report.%rsc
   'product-grandparent
   #'(lambda (item) (when (and (parent item) (parent (parent item)))
                      (name (parent (parent item))))))
  ;; return name of 2 level group(counting from root, root group has 1 level),
  ;; which is ancestor of given item
  (report.%rsc
   'product-2-lvl-group
   #'(lambda (item) (loop
                       :for cur := (parent item) :then (parent cur)
                       :while (and cur (parent cur) (parent (parent cur)))
                       :finally (return (when (and cur (parent cur)) (name cur))))))
  (report.%rsc
   'product-secret (rcurry #'get-option "Secret" "Checked"))
  (report.%rsc
   'product-dtd
   #'(lambda (item)
       (gethash (articul item) *xls.product-table*)))
  (report.%rsc 'product-vendor #'vendor)
  (report.%rsc 'product-delivery #'get-product-delivery-price)
  (report.%rsc
   'product-seria (rcurry #'get-option "Общие характеристики" "Серия"))
  (report.%rsc
   'product-direct-name (rcurry #'get-option "Secret" "Direct-name"))
  (report.%rsc
   'product-double (rcurry #'get-option "Secret" "Дубль"))
  (report.%rsc
   'product-warranty (rcurry #'get-option "Дополнительная информация" "Гарантия"))
  (report.%rsc
   'product-url
   #'(lambda (item)
       ;; TODO: use restas url designator
       (format nil "http://www.320-8080.ru/~A" (key item))))
  ;; group functions
  (report.%rsc
   'group-name #'name)
  (report.%rsc
   'group-ymlshow #'ymlshow)
  (report.%rsc
   'group-url
   #'(lambda (item)
       ;; TODO: use restas url designator
       (format nil "http://www.320-8080.ru/~A" (key item))))
  (report.%rsc
   'group-active #'(lambda (item) (if (active item) "yes" "no")))
  (report.%rsc
   'group-seo-text
   #'(lambda (item)
       (if (valid-string-p (seo-text item))
           "yes" "no")))
  (report.%rsc
   'group-count-products
   #'(lambda (item)
       (length (products item))))
  (report.%rsc
   'group-count-active-products
   #'(lambda (item)
       (count-if #'active (products item))))
  (report.%rsc
   'group-rootgroup-name
   #'(lambda (item) (aif (get-root-parent item)
                         (name it)
                         "no")))
  ;; marketing filters
  (report.%rsc 'filter-key #'key)
  (report.%rsc 'filter-active #'(lambda (item) (if (active item) "да" "нет")))
  (report.%rsc 'filter-vendor #'(lambda (item) (getf (data item) :vendor)))
  (report.%rsc 'filter-seria #'(lambda (item) (getf (data item) :seria)))
  (report.%rsc 'filter-name #'(lambda (item) (getf (data item) :name)))
  (report.%rsc 'filter-seotext #'(lambda (item) (if (valid-string-p (seo-text item))
                                                    "есть"
                                                    "нет")))
  (report.%rsc
   'filter-url
   #'(lambda (item)
       ;; TODO: use restas url designator
       (if (parent item)
           (format nil "http://www.320-8080.ru/~A/~A"
                   (key (parent item))
                   (key item))
           ;; else
           "no url")))
  (report.%rsc
   'filter-products
   #'(lambda (item) (length (filters.filter item))))
  (report.%rsc
   'filter-active-products
   #'(lambda (item) (count-if #'active (filters.filter item))))
  ;; printers/cartriges
  (report.%rsc 'printer-name #'name)
  (report.%rsc 'printer-vendor #'vendor)
  (report.%rsc 'printer-type #'printer-type)
  (report.%rsc 'printer-original-cartr-count
               #'(lambda (printer) (format nil "~D" (length (original-cartriges printer)))))
  (report.%rsc 'printer-original-cartr-active
               #'(lambda (printer) (format nil "~D" (count-if #'active (original-cartriges printer)
                                                         :key (alexandria:rcurry #'getobj 'product)))))
  (report.%rsc 'printer-other-cartr-count
               #'(lambda (printer) (format nil "~D" (length (other-cartriges printer)))))
  (report.%rsc 'printer-other-cartr-active
               #'(lambda (printer) (format nil "~D" (count-if #'active (other-cartriges printer)
                                                         :key (alexandria:rcurry #'getobj 'product)))))
  (report.%rsc 'printer-all-cartr-count
               #'(lambda (printer) (format nil "~D"
                                      (+ (length (original-cartriges printer))
                                         (length (other-cartriges printer))))))
  (report.%rsc 'printer-all-cartr-active
               #'(lambda (printer) (format nil "~D"
                                      (+ (count-if #'active (original-cartriges printer)
                                                   :key (alexandria:rcurry #'getobj 'product))
                                         (count-if #'active (other-cartriges printer)
                                                   :key (alexandria:rcurry #'getobj 'product))))))
  (report.%rsc 'printer-original-cartriges
               #'(lambda (printer) (format nil "~{~A~^,~}" (original-cartriges printer))))
  (report.%rsc 'printer-other-cartriges
               #'(lambda (printer) (format nil "~{~A~^,~}" (other-cartriges printer)))))

(report.register-standard-columns)

(defun report.product-report (stream)
  (report.write-report-with-standard-columns
   stream
   '(("артикул" . product-articul)
     ("цена магазина" . product-price)
     ("цена сайта" . product-siteprice)
     ("имя" . product-name)
     ("имя real" . product-name-real)
     ("имя yml" . product-yml-name)
     ("is-yml-show" . product-yml-show)
     ("seo текст" . product-seo-text-exists)
     ("фотографии" . product-num-pics)
     ("характеристики" . product-valid-options)
     ("активный" . product-active)
     ("группа" . product-group)
     ("родительская группа" . product-grandparent)
     ("группа 2-го уровня" . product-2-lvl-group)
     ("secret" . product-secret)
     ("DTD" . product-dtd)
     ("vendor" . product-vendor)
     ("доставка" . product-delivery)
     ("серия" . product-seria)
     ("direct-name" . product-direct-name)
     ("дубль" . product-double)
     ("гарантия" . product-warranty)
     ("1с группа" . product-erp-class)
     ("1с цена" . product-erp-price))
   'product))

(defun report.group-report (stream)
  (report.write-report-with-standard-columns
   stream
   '(("Название категории" . group-name)
     ("url страницы" . group-url)
     ("Active" . group-active)
     ("YmlShow" . group-ymlshow)
     ("seo-text" . group-seo-text)
     ("продуктов" . group-count-products)
     ("активных" . group-count-active-products))
   'group))

(defun report.seo-seria-filters (stream)
  (report.write-report-with-standard-columns
   stream
   '(("Брэнд" . filter-vendor)
     ("Серия" . filter-seria)
     ("Имя" . filter-name)
     ("URL" . filter-url)
     ("продуктов" . filter-products)
     ("активных" . filter-active-products)
     ("описание" . filter-seotext))
   (marketing-filters.get-seria-filters)))

(defun report.groups-products-report (stream)
  (report.write-report-with-standard-columns
   stream
   '(("Ключ" . item-key)
     ("Название root категории" . group-rootgroup-name)
     ("Название группы" . group-name)
     ("Продуктов" . group-count-products)
     ("Активных продуктов" . group-count-active-products))
   'group))

(defun report.product-vendor-report (stream)
  (report.write-report-with-standard-columns
   stream
   '(("Название категории" . product-group)
     ("Брэнд" . product-vendor)
     ("Название товара" . product-name-real)
     ("url страницы" . product-url)
     ("Active" . product-active)
     ("seo-text" . product-seo-text-exists))
   'product))

(defun report.printers-report (stream)
  (report.write-report-with-standard-columns
   stream
   '(("Название принтера" . printer-name)
     ("Производитель" . printer-vendor)
     ("Тип" . printer-type)
     ("Количество (оригинальные)" . printer-original-cartr-count)
     ("Активных (оригинальные)" . printer-original-cartr-active)
     ("Количество (другие)" . printer-other-cartr-count)
     ("Активных (другие)" . printer-other-cartr-active)
     ("Количество (все)" . printer-all-cartr-count)
     ("Активных (все)" . printer-all-cartr-active)
     ("Оригинальные картриджи" . printer-original-cartriges)
     ("Другие картриджи" . printer-other-cartriges))
   (alexandria:hash-table-values *printer-storage*)))

(defun report.filters-report (stream)
  (report.write-report-with-standard-columns
   stream
   '(("Key" . filter-key)
     ("Имя" . filter-name)
     ("Url" . filter-url)
     ("Активный" . filter-active)
     ("Количество продуктов" . filter-products)
     ("Количество активных продуктов" . filter-active-products))
   'filter))

(defun report.pics-report (stream &optional (products nil products-supplied-p))
  (cl-csv:write-csv-row (list "Продукт"
                              "имя"
                              "ширина"
                              "высота"
                              "размер (Кб)")
                        :stream stream)
  (mapcar
   #'(lambda (product)
       (mapcar
        #'(lambda (pic)
            (let* ((pic-path (pic-path (key product) pic))
                   (dimensions (get-dimensions pic-path))
                   (size (with-open-file (file pic-path) (file-length file))))
              (cl-csv:write-csv-row (list (key product)
                                          pic
                                          (getf dimensions :width)
                                          (getf dimensions :height)
                                          (floor size 1000)) ; in Kb
                                    :stream stream)))
        (get-pics (key product))))
   (if products-supplied-p
       products
       (collect-storage 'product)))
  (values))

(defun write-vendors (stream)
  (format stream "~a;~a;~a;~a;~a;~a;~a;~%"
          "Название категории"
          "Брэнд"
          "url страницы"
          "Active"
          "seo-text"
          "продуктов"
          "активных")
  (process-storage
   #'(lambda (v)
       (unless (groups v)
         (maphash #'(lambda (vendor num)
                      (declare (ignore num))
                      (let ((products
                             (remove-if-not #'(lambda (p)
                                                (vendor-filter-controller p vendor))
                                            (products v))))
                        (format stream "\"~a\";\"~a\";http://www.320-8080.ru/~a?vendor=~a;~a;~a;~a;~a;~%"
                                (stripper (name v))
                                (stripper vendor)
                                (hunchentoot:url-encode (key v))
                                (hunchentoot:url-encode (stripper vendor))
                                "yes"
                                (if (classes.has-vendor-seo-text v vendor)
                                    "yes"
                                    "no")
                                (length products)
                                (count-if #'active products))))
                  (storage.get-vendors (storage.get-filtered-products v #'atom)))))
   'group))

(defun create-report (file-name report-func)
  (log:info "Create report ~A ..." file-name)
  (let ((filepath (merge-pathnames file-name (config.get-option :paths :path-to-reports))))
    (with-open-file
        (stream filepath :direction :output :if-exists :supersede :external-format :cp1251)
      (print stream)
      (funcall report-func stream))))


(defun post-proccess-gateway ()
  (mapcar #'(lambda (v)
              (let ((p (getobj v 'product)))
                (when p
                  (setf (preorder p) t)
                  (setf (active p) t)
                  (setobj v p)
                  (setf (gethash v *special-products*) p))))
          (list
           "666616"
           "711265"
           "834786"
           "938111"
           "777888"
           "888777"
           "999111"
           "999777"))
  (let ((rs))
    (process-storage
     #'(lambda (v)
         (when (and
                (active v)
                (not (preorder v))
                (zerop (siteprice v)))
           (push v rs)
           (setf (active v) nil)))
     'product)
    (length rs))
  (black-list.clear)
  (black-list.deactivate-all))

(defun product-delivery (p)
  (let ((g (parent p))
        (daily (gethash (articul p) (daily *main-page.storage*))))
    (if daily
        0
        (aif (delivery-price p)
             it
             (if (and g (delivery-price g))
                 (delivery-price g)
                 300)))))

(defun report.delete-doubles (products)
  (mapcar #'(lambda (v)
              (remobj (format nil "~A" v) 'product))
          products))

(defun report.add-products-to-group (product-list gr)
  (mapcar #'(lambda (v)
              (let ((pr (getobj (format nil "~a" v) 'product)))
                (when pr
                  (setf (parents pr) (list gr))
                  (push pr (products gr)))))
          product-list)
  "done")

(defun report.convert-name (input-string)
  (string-trim (list #\Space #\Tab #\Newline)
               (format nil "~:(~a~)" input-string)))

(defun report.do-seo-reports ()
  (let ((time (time.encode.backup-filename)))
    (create-report (format nil "seo-report-groups-~A.csv" time) #'report.group-report)
    (create-report (format nil "seo-report-vendors-~A.csv" time) #'write-vendors)
    ;; FIXME: wrong names for reports
    (create-report (format nil "seo-report-products-~A.csv" time) #'report.seo-seria-filters)
    (create-report (format nil "seo-report-seria-filters-~A.csv" time) #'report.product-vendor-report)
    ;; FIXME: do own button and fuction for the printers and filters reports
    (create-report (format nil "printer-report-~A.csv" time) #'report.printers-report)
    (create-report (format nil "filter-report-~A.csv" time) #'report.filters-report)))


(defun report.write-alias (&optional (stream *standard-output*))
  (format stream "имя группы; наличие алиасов; группа опций ; опция; имя алиаса; ед. измерения;")
  (collect-storage
   'group
   :func #'(lambda (gr)
             (if (null (catalog-keyoptions gr))
                 (format stream "~&~a; нет;" (name gr))
                 (mapcar #'(lambda (alias)
                             (let ((alias-temp (mapcar #'stripper
                                                       (remove-if #'keywordp alias))))
                               (format stream "~&\"~a\";~a;~{\" ~a \";~}"
                                       (stripper (name gr))
                                       (if alias-temp "есть" "нет")
                                       alias-temp)))
                         (catalog-keyoptions gr))))))

(defun report.write-keyoptions (&optional (stream *standard-output*))
  (format stream "имя группы; наличие ключевых опций; группа опций ; опция;")
  (collect-storage
   'group
   :func #'(lambda (gr)
             (if (null (keyoptions gr))
                 (format stream "~&~a; нет;" (name gr))
                 (mapcar #'(lambda (alias)
                             (let ((alias-temp (mapcar #'stripper
                                                       (remove-if #'keywordp alias))))
                               (format stream "~&\"~a\";~a;~{\" ~a \";~}"
                                       (stripper (name gr))
                                       (if alias-temp "есть" "нет")
                                       alias-temp)))
                         (keyoptions gr))))))


(defun report.do-alias-reports ()
  (let ((time (time.encode.backup-filename)))
    (create-report (format nil "aliases-report-~A.csv" time) #'report.write-alias)
    (create-report (format nil "keyoptions-report-~A.csv" time) #'report.write-keyoptions)))

(defun report.do-groups-products-report ()
  (create-report (format nil "groups-products-~a.csv" (time.encode.backup-filename))
                 #'report.groups-products-report))


;;;; fullfilter report
;; temp
(defun fullfilter-keyword-t (keyword)
  (anything-to-keyword (format nil "~A-T" keyword)))

(defun fullfilter-keyword-f (keyword)
  (anything-to-keyword (format nil "~A-F" keyword)))

(defun fullfilter-keyword-n (keyword num)
  (anything-to-keyword (format nil "~A-~D" keyword num)))

(defun get-advanced-filters (group)
  (loop :for filter-group :in (advanced (fullfilter group))
     :appending (second filter-group) :into filters
     :finally (return filters)))

(defun fullfilter-report (stream)
  (format stream "Группа;Ключ группы;Фильтр;Значение;Продуктов;Активных;~%")
  (process-storage
   #'(lambda (group)
       (when (fullfilter group)
         (let ((filters (append (base (fullfilter group)) (get-advanced-filters group)))
               res)
           (mapcar #'(lambda (param-list)
                       (let ((kw (first param-list))
                             (name (second param-list)))
                         (if (or (equal :slider (third param-list))
                                 (equal :range (third param-list)))
                             (let ((products (fullfilter-controller (products group) group
                                                                    (list (fullfilter-keyword-f kw) "0"
                                                                          (fullfilter-keyword-t kw) "100000000"))))
                               (push (list name (length products) (count-if #'active products))
                                     res))
                             ;; else
                             (loop :for opt :in (fourth param-list)
                                :for i :from 0 to (1- (length (fourth param-list)))
                                :do
                                (let ((products (fullfilter-controller (products group) group
                                                                       (list (fullfilter-keyword-n kw i) "1"))))
                                  (push (list name opt (length products) (count-if #'active products))
                                        res))))))
                   filters)
           (mapcar #'(lambda (res-val)
                       (format stream "~A;~A;~{~A;~}~%"
                               (name group) (key group) res-val))
                   res))))
   'group))
