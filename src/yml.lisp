;;;; yml.lisp

(in-package #:eshop)

;; Группы представляют собой лес для YML нам нужны не только сами
;; группы маркированные для выгрузки но и их родители
;; На самом деле нам нужно минимальные остовные деревья,
;; но будем выгружать полные деревья исключая только листья, если это нужно

(defparameter *yml-group-ids* (make-hash-table :test #'equal))

(defun yml-groups ()
  "Строим *yml-group-ids*"
  (let ((current-id 1))
    (clrhash *yml-group-ids*)
    (process-storage
     #'(lambda (gr)
         ;; Если группа имеет дочерние группы ИЛИ yml-show == true
         (when (or (groups gr) (ymlshow gr))
           ;; Кладем ее в *yml-group-ids* и увеличиваем current-id
           (setf (gethash (key gr) *yml-group-ids*) current-id )
           (incf current-id)))
     'group)
    *yml-group-ids*))

(defun yml.yml-show-p (product)
  (declare (product product))
  (and (parent product)
       (ymlshow (parent product))
       (active product) ;; (yml.available-for-order-p product))
       (price product)
       (plusp (price product))
       ;;для селективного исключения товаров по значению специальной опции
       (let ((yml-show (get-option product "Secret" "YML")))
         (not (and yml-show
                   (string= "No" (stripper yml-show)))))))

(defun yml.is-daily-product (product)
  (loop
     :for v :being :the hash-values :in (daily *main-page.storage*)
     :thereis (and (equal (key v) (key product))
                   (< (date-start v) (get-universal-time) (date-finish v)))))

(defun yml.%offers ()
  (format nil "~{~a~}"
          (collect-storage
           'product
           ;;продукт должен находиться в группе маркированной как ymlshow
           ;;быть активным и иметь не нулевую цену
           :when-fn #'yml.yml-show-p
           :func #'(lambda (product)
                     (soy.yml:offer
                      (list :articul (articul product)
                            :available (active product) ; если не active, то прошел available-for-order
                            :deliveryprice (get-product-delivery-price product)
                            :price (siteprice product)
                            :category (yml-id (parent product))
                            :picture (let ((pics (get-pics
                                                  (key product))))
                                       (when pics
                                         (encode-uri (car pics))))
                            :vendorcode (get-option product "Общие характеристики" "Код производителя")
                            :name (let ((yml-name (get-option product "Secret" "Yandex")))
                                    (if (or (null yml-name)
                                            (string= "" (stripper yml-name))
                                            (string= "No" (stripper yml-name)))
                                        (name-seo product)
                                        yml-name))
                            :description nil))))))

(defun yml.%category ()
  (loop
     :for key
     :being :the hash-key
     :in (yml-groups)
     :when (getobj key 'group)
     :collect (let ((obj (getobj key 'group)))
                (list :id (yml-id obj)
                      :name (name obj)
                      :parent (if (null (parent obj))
                                  0 ; если это вершина дерева
                                  (yml-id (parent obj)))))))

(defun yml.%flush (data)
  (let* ((name (concatenate 'string "yml" "-" (time.encode.backup-filename) ".xml"))
         (yml-path (merge-pathnames #P"yml/" (config.get-option :paths :path-to-backups)))
         (yml-file (merge-pathnames name yml-path)))
    (ensure-directories-exist yml-file)
    (with-open-file (file yml-file
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :external-format :utf-8)
      (format file "~&~A~%" data))))

(defun yml-page ()
  (let ((data-yml))
    (setf (hunchentoot:content-type*) "application/xml; charset=utf-8")
    (setf data-yml (soy.yml:xml
                    (list :datetime (time.get-date-time)
                          :marketname "ЦиFры 320-8080"
                          :marketcompany "ЦиFры 320-8080"
                          :marketurl "http://www.320-8080.ru/"
                          :categoryes (yml.%category)
                          :offers (yml.%offers))))
    (when (search "YandexMarket" (tbnl:user-agent))
      (bt:make-thread #'(lambda () (yml.%flush data-yml))
                      :name "flash-yml"))
    data-yml))

(defun yml.get-next-yml-id ()
  "Generate uniq group id for yanedx market. Max current id +1."
  (if (not (get-storage 'group))
      0
      (let ((max 0))
        (process-storage #'(lambda (gr)
                             (let ((id (yml-id gr)))
                               (when (and id (< max id))
                                 (setf max id))))
                         'group)
        (1+ max))))

(defun yml.available-for-order-p (product)
  (declare (product product))
  ;; life-time is given in days
  (let ((parent (parent product)))
    (when (and parent (life-time parent) (plusp (life-time parent)))
      (< (get-universal-time) (+ (date-modified product)
                                 (* 60 60 24 (life-time parent)))))))


(defun yml.count-products-for-order (&optional group)
  "Count number of products, which are not active, but available for order by calling"
  (declare ((or group null) group))
  (let ((products (if group
                      (storage.get-recursive-products group (complement #'active))
                      (collect-storage 'product :when-fn (complement #'active)))))
    (count-if #'yml.available-for-order-p products)))

(defun yml.pretty-count-products-for-order ()
  "Count number of products, which are not active, with pretty print for all groups"
  (format nil "~{~A~%~}"
          (mapcar #'(lambda (gr)
                      (format nil "Group: ~A, products for order: ~D"
                              (key gr)
                              (yml.count-products-for-order gr)))
                  (sort
                   (collect-storage 'group
                                    :when-fn
                                    #'(lambda (gr)
                                        (plusp (yml.count-products-for-order gr))))
                   #'< :key #'yml.count-products-for-order))))

(defun yml.get-list-for-order (group)
  (declare (group group))
  (remove-if-not #'yml.available-for-order-p
                 (storage.get-recursive-products group (complement #'active))))
