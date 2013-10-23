;;;; gateway.lisp

(in-package #:eshop)

(defclass gateway.dump ()
  ((products-num :initform 0 :accessor product-num)
   (is-loaded :initform nil :accessor is-loaded)
   (date :initform 0 :accessor date))
  (:documentation "Information about last product import dump from ERP (or another place)"))

(defclass gateway.erp-data-dump (gateway.dump)
  ((list-raw-data :initform nil :accessor list-raw-data))
  (:documentation "Raw data list of import products from ERP (or another place)"))

(defvar *gateway.loaded-dump* (make-instance 'gateway.dump)
 "Information about last loaded product dump")

(defvar *gateway.dump* (make-instance 'gateway.erp-data-dump)
  "Information about current import from ERP (or another place)")

(defvar *gateway.import-time* nil)

(defun %gateway.clear-dump (&optional (dump *gateway.dump*))
  "Init dump with new class gateway.erp-data-dump instance"
  (declare (gateway.erp-data-dump dump))
  (setf (list-raw-data dump) nil))

(defun %gateway.add-data->dump (data &optional (dump *gateway.dump*))
  "Add raw ERP products data to dump"
  (declare (gateway.erp-data-dump dump))
  (push data (list-raw-data dump)))

(defun %gateway.processing-first-package (data &optional (dump *gateway.dump*))
  "Prepare dump, start to collect packet data & give back answer"
  (declare (gateway.erp-data-dump dump))
  (%gateway.clear-dump)
  (%gateway.add-data->dump data dump)
  "first")

(defun %gateway.processing-package (data &optional (dump *gateway.dump*))
  "Collect packet data & give back answer"
  (declare (gateway.erp-data-dump dump))
  (%gateway.add-data->dump data dump)
  "ordinal")

(defun gateway.make-dump-pathname (&optional (timestamp (get-universal-time)))
  "Create pathname dump file (named yyyy-mm-dd_HH:MM:SS.bkp) uses config :PATHS :path-to-gateway"
  (declare (number timestamp))
  (let* ((filename (concatenate 'string (time.encode.backup timestamp) ".bkp"))
         (dump-path (config.get-option :paths :path-to-gateway)))
    (merge-pathnames filename dump-path)))

(defun %gateway.get-time-from-dumpfile (dump-name)
  "Extract time from dump file name (named yyyy-mm-dd_HH:MM:SS.bkp)"
  (declare (pathname dump-name))
  (time.decode.backup (pathname-name dump-name)))

(defun %gateway.search-dump-path-and-timestamp (&optional (timestamp (get-universal-time)))
  "Looking for dump file named with ts near timestamp & give back (values filename ts) (NIL NIL) for empty dump-path"
  (declare (number timestamp))
  (let* ((dump-path (config.get-option :paths :path-to-gateway))
         (dump-mask (concatenate 'string (namestring dump-path) "*.bkp"))
         (current-filename (gateway.make-dump-pathname timestamp))
         (result-file nil)
         (result-time nil))
    (flet ((pathname> (pathname1 pathname2 &key (start1 0) end1 (start2 0) end2)
             (string> (namestring pathname1) (namestring pathname2)
                      :start1 start1 :start2 start2 :end1 end1 :end2 end2)))
      (setf result-file (first (sort (remove-if (rcurry #'pathname> current-filename)
                                                (directory dump-mask)) #'pathname>)))
      (awhen result-file
           (setf result-time (%gateway.get-time-from-dumpfile it)))
      (values result-file result-time))))

(defun %gateway.prepare-raw-data (raw)
  "Сonvert raw data to presentable format"
  (slots.string-delete-newlines
   (sb-ext:octets-to-string raw :external-format :cp1251)))

(defun gateway.flush-dump (raw &optional (timestamp (get-universal-time)))
  "Flush dump data list to file"
  (declare (number timestamp))
  (with-open-file (file (gateway.make-dump-pathname timestamp)
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create
                        :external-format :utf-8)
    (mapcar #'(lambda (data)
                (format file "~&~A~%"
                        (%gateway.prepare-raw-data data)))
            (reverse raw))))

(defun %gateway.decode-json-from-file (pathname)
  "Read file and decode json data to appended list"
  (declare (pathname pathname))
  (dolines (line pathname result)
    (setf result (nconc (st-json:read-json-from-string line) result))))

 (defun %product-update-name (product name)
  "Update product field name"
  (declare (product product) ((or string t) name))
  (when name
    (setf (name-provider product) name))
  (when (string= "" (name-seo product))
    (setf (name-seo product) name)))

(defun %product-update-prices (product siteprice price oldprice)
  "Update product fields delta-price & siteprice"
  (declare (product product) ((or number t) siteprice price))
  ;; пересчет дельты если пришла только цена сайта
  (when (and siteprice
             (not price))
    (setf (delta-price product) (- (price product) siteprice)))
  ;; цены
  (when siteprice
    (setf (siteprice product) siteprice))
  ;; перещет дельты не проводится для акционных товаров
  (when (and price
             (not (groupd.is-groupd product)))
    (setf (delta-price product) (- price (siteprice product))))
  ;; старая цена для аукционных товаров
  ;; когда розничная цена и цена сайта совпадают, но необходимо
  ;; отображать разницу
  (when (and (groupd.is-groupd product)
             oldprice
             (zerop (delta-price product))
             (plusp oldprice))
    ;; (log:info "old: ~A" product)
    (setf (delta-price product) (- oldprice (siteprice product)))))

(defun %product-update-bonuscount (product bonuscount)
  "Update product fields bonuscount"
  (declare (product product) ((or number t) bonuscount))
  ;;TODO:(возможно нужно пересчитывать когда приходит новая цена)
  (when bonuscount
    (setf (bonuscount product) bonuscount)))

(defun %product-update-erp (product erp-price erp-class)
  "Update product fields erp-price erp-class"
  (declare (product product) ((or number t) erp-price erp-class))
  (when erp-price
    (setf (erp-price product) erp-price))
  (when erp-class
    (setf (erp-class product) erp-class)))

(defun %product-update-counts (product count-total count-transit)
  "Update product fields count-total count-transit"
  (declare (product product) ((or number t) count-total count-transit))
  (aif count-total
       (setf (count-total product) it)
       (when (and count-transit
                  (zerop count-transit)
                  (= (count-total product)
                     (count-transit product)))
         (black-list.insert product)
         (setf (count-total product) 0)))
  (when count-transit
    (setf (count-transit  product) count-transit)))

(defun %gateway.process-product (item)
  "Process product item plist. Check fields and update data in storage."
  (labels ((@ (field) (getf item field))
           (fl@ (field) (float-string->int (@ field))))
    (awhen (@ :id)
      (let* ((key (write-to-string (fl@ :id)))
             (old-product (getobj key 'product))
             (product))
        (setf product (aif old-product
                           it
                           (make-instance 'product :articul (fl@ :id))))
        (setf (key product) key)
        (setf (articul product) (fl@ :id))
        (%product-update-name product (@ :name))
        (%product-update-prices product
                                (fl@ :price--site) (fl@ :price) (fl@ :price--old))
        (%product-update-bonuscount product (fl@ :bonuscount))
        (%product-update-counts product (fl@ :count--total) (fl@ :count--transit))
        (%product-update-erp product (fl@ :iprice) (@ :cat))
        (setf (active product) (plusp (count-total product)))
        (unless old-product
          (setobj (key product) product))))))

(defun %gateway.process-products-dump-data (items)
  "Process list items. Where items a alist example: ((:ID . \"158354\") (:NAME . \"USB HUB 4port mobileData HB-65\") (:ISNEW . \"0\"))"
  (loop :for item :in items
     :do (%gateway.process-product (alist-plist item))))

(defun %gateway.update-actives (items)
  "Update actives for products not entered the itmes"
  (let ((articuls (make-hash-table :test #'equal)))
    (mapcar #'(lambda (v)
                (let ((articul (write-to-string (float-string->int (cdr (assoc :id v))))))
                  (setf (gethash articul articuls) t)))
            items)
    (process-storage #'(lambda (v)
                         (when (and (not (gethash (key v) articuls))
                                    (active v))
                           (setf (active v) nil)
                           (setf (count-total v) 0)
                           (setf (count-transit v) 0)))
                     'product)))

(defun %gateway.singles-pathname ()
  (merge-pathnames "singles.txt" (config.get-option :paths :path-to-gateway)))

(defun gateway.restore-singles (dump-timestamp &optional (current-timestamp (get-universal-time)))
  "Load singles products witch came between dump-timestamp and current-timestamp"
  (declare (number dump-timestamp current-timestamp))
  (let ((data))
    (labels ((@time (line) (subseq line 0 19))
             (@json (line) (subseq line 21))
             (@validp (line) (<= dump-timestamp (time.decode.backup (@time line))
                                current-timestamp)))
      (dolines (line (%gateway.singles-pathname))
        (when (@validp line)
          (let ((*gateway.import-time* (time.decode.backup (@time line))))
            (setf data (st-json:read-json-from-string (@json line)))
            (%gateway.process-products-dump-data data)))))))

(defun gateway.%process-data (data last-dump-ts &optional (timestamp (get-universal-time)))
  "Process products data and do post proccess"
  (declare (number last-dump-ts timestamp))
  (setf (date *gateway.loaded-dump*) last-dump-ts)
  (setf (product-num *gateway.loaded-dump*) (length data))
  (%gateway.process-products-dump-data data)
  (%gateway.update-actives data)
  (gateway.restore-singles last-dump-ts timestamp)
  (post-proccess-gateway))

(defun %gateway.load-data (last-dump last-dump-ts &optional (timestamp (get-universal-time)))
  "Load product data from decoded alists"
  (declare (number last-dump-ts timestamp))
  (let ((data (%gateway.decode-json-from-file last-dump)))
    (gateway.%process-data data last-dump-ts)))

(defun gateway.%load-dump (raw)
  "Load products from raw"
  (let ((data (loop
                  :for line :in raw
                  :nconc (st-json:read-json-from-string
                          (%gateway.prepare-raw-data line))))
        (last-dump-ts (get-universal-time)))
    (gateway.%process-data data last-dump-ts)))

(defun gateway.load (&optional (timestamp (get-universal-time)))
  "Load products from file dump"
  (declare (number timestamp))
  (multiple-value-bind (last-dump last-dump-ts) (%gateway.search-dump-path-and-timestamp timestamp)
    (awhen last-dump
      (%gateway.load-data last-dump last-dump-ts timestamp))))

(defun gateway.%store-and-processed-dump (raw)
  "Store data, load and processed data"
  (gateway.flush-dump raw)
  (gateway.%load-dump raw))

(defun %gateway.processing-last-package (data &optional (dump *gateway.dump*))
  "Finish data collecting, start separate proccess to store and load dump give back answer"
  (%gateway.add-data->dump data dump)
  (setf (date dump) (get-universal-time))
  (let ((raw (list-raw-data dump)))
    (bt:make-thread #'(lambda () (gateway.%store-and-processed-dump raw))
                    :name "store-and-processed-dump"))
  (%gateway.clear-dump)
  "last")

(defun gateway.store-single-gateway (data &optional (timestamp (get-universal-time)))
  "Сохраняет одиночные выгрузки в файл"
  (with-open-file (file (%gateway.singles-pathname)
                        :direction :output
                        :if-exists :append
                        :if-does-not-exist :create
                        :external-format :utf-8)
    (format file "~&~A>>~A~%" (time.encode.backup timestamp) data)))

(defun %gateway.processing-single-package (raw)
  "Обработка одиночного изменения, для экстренного внесения изменений на небольшое количество товаров"
  (let ((*gateway.import-time* (get-universal-time))
        (data (%gateway.prepare-raw-data raw)))
    (gateway.store-single-gateway data)
    (%gateway.process-products-dump-data (st-json:read-json-from-string data))
    ;; возможно тут необходимо пересчитать списки активных товаров или еще что-то
    "single"))

(defun gateway-page ()
  "GP"
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
  (let ((num (format nil "~A" (hunchentoot:get-parameter "num")))
        (single (format nil "~A" (hunchentoot:get-parameter "single"))))
    (aif (hunchentoot:raw-post-data)
         (switch (num :test #'string=)
           ("1" (%gateway.processing-first-package it))
           ("0" (%gateway.processing-last-package it))
           (t (switch (single :test #'string=)
                ("1" (%gateway.processing-single-package it))
                (t (%gateway.processing-package it)))))
         "NIL")))

;;;; EKK

(defun gateway-ekk ()
  "Только одиночная выгрузка. Номера карт и бонусы, разделённые пробелами"
  (let* ((raw (sb-ext:octets-to-string (hunchentoot:raw-post-data)))
         (data (split-sequence #\Space raw)))
    (doplist (k v data)
      (let ((v (parse-integer v)))
        (aif (eshop.odm:getobj 'bonuscard k)
             (eshop.odm:setobj it 'count v)
             (make-instance 'bonuscard :key k :count v))))
    "ok"))
