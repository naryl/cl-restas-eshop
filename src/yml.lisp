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

(defun yml.get-delivery-price (cart)
  (let ((result 300)
        (min-price 500)
        (max-price 0))
    (mapcar #'(lambda (v)
                (let* ((product (getobj (getf v :articul) 'product))
                       (d (yml.get-product-delivery-price1 product)))
                  (if (> d max-price)
                      (setf max-price d))
                  (if (< d min-price)
                      (setf min-price d))))
            cart)
    (if (= max-price 500)
        (setf result 500)
        (setf result min-price))
    result))


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
                            :deliveryprice (yml.get-product-delivery-price1 product)
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

(defun yml.get-product-delivery-price1 (product)
  (let ((parent (if product (parent product)))
        (key)
        (result 300))
    (when parent
      (setf key (key parent))
      (if (or
           (search (list (articul product))
                   '(208272 208273 208274 208277 208275 208276 208290 208332 208333 208291 208292
 208334 208329 208285 208286 208293 208294 208295 208296 208297 208298 208335
 208287 208330 208288 208289 208331 208281 208282 208283 208284 208279 208280
 208278 208305 208306 208307 208308 208299 208300 208301 208302 208303 208309
 208311 208312 208313 208314 208315 208316 208327 208328 208318 208319 208320
 208321 208322 208323 208324 208325 208317 217913 217912 218001 218000 218003
 218005 218004 218002 218006 217931 217932 217934 217937 217925 217926 217918
 217916 217919 217989 217993 217994 217984 217983 217944 217942 219475 217943
 217946 217981 217979 217982 217980 217985 217966 217964 217965 219474 217992
 217991 217967 217968 217929 217930 217978 217977 217996 219480 217960 217961
 219479 217963 219481 217962 217988 217999 217998 217997 217986 217987 217975
 217976 217949 217951 217950 217952 217935 217933 217939 217940 217936 217938
 217941 217928 217927 219484 219482 219485 219483 217947 217948 217973 217974
 217959 219476 217958 217956 217955 219478 219477 217924 217921 217920 217922
 217923 217971 217969 217972 217970 217917 217915 219473 218024 218017 218016
 218018 218023 218022 218031 218032 218033 218021 218020 218019 218027 218028
 218030 218029 218025 218012 218014 218007 218015 218010 218011 218009 219469
 219468 219470 219472 219471 219487 219489 219497 219498 219493 219494 219492
 219491 168500 172230 184019 209304 215282 214794 164514 197576 172884 209153
 209168 209178 209193 209204 169396 199783 201792 184004 169427 166679 215191
 173232 167764 197726 197729 197761 201790 201787 197778 182187 182703 164862
 164863 164866 164955 194869 166684 192298 167046 167051 183984 183985 167202
 164853 215178 209097 185912 198046 200219 190956 194829 167196 190967 165060
 165063 172862 192971 200271 170322 200282 164584 170363 165062 205289 215138
 190315 164634 202465 192413 190922 200198 172850 200206 164824 199792 186397
 173233 164898 199795 169377 172264 179939 179927 179928 179930 179929 208909
 203025 192950 157312 155463 163786 211126 160420 156749 163559 169303 175882
 174810 156881 209261 205324 157361 155663 205330 203805 165429 192359 202123
 155996 163986 169263 169440 209026 182721 214664 207500 207504 207503 207502 216730 207501
                     216440 216442 216443 216444 216445 216446 216447 216448 216449 216450 216451
 216453 216455 216454 216456 216457 216458 216460 216461 216474 216476 216477
 216478 216480 216481 216482 216483 216484 216486 216487 216488
                     156568 156569 156570 156572 165661 157455 156574 156575 185722 162720 158992
 156561 156562 157452 156565 156566 155411 156590 156592 192443 157453 183921
 162554 183922 165675 156609 158994 198082 156567 165653 165654 165655 156555
 160399 169259 176499 165209 176500 167269 156510 156511 156514 156515 160177
 180074 194886 194887 197644 157322 155480 155483 156732 185728 157475 157328
 155520 158453 165665 160186 156745 169263 180366 174770 198091 202088 165669
 167818 162547 180365 185725 182723 163824 169303 163816 163818 170535 181986
 193220 193222 157307 155421 155423 155424 155425 155426 155427 159191 158260
 161275 155473 155474 158468 158466 157448 163986 162670 162671 167301 156015
 163546 156016 157378 218289
                     216709
                     218306
                     218310
                     218311
                     218325
                     218327
                     220143
                     219301
                     219803
                     219359
                     219364
198792
187603
187686
204859
187676
187742
187751
187773
187788
187780
                     215840
217077
215835
215836
217083
217082
218886
189750
148365
190892
162838
                     ))
           (let ((diagonal (get-option product "Экран" "Диагональ экрана, дюйм")))
             (when (equal diagonal "")
               (setf diagonal nil))
             (setf diagonal (ceiling (parse-float diagonal)))
             (and (>= diagonal 32)
                  (string= (vendor product) "Samsung")))
           (equal key "kondicioneri")
           ;; (< 3000 (siteprice product))
           ;; (equal key "komputery")
           ;; (and (equal (vendor product) "Brother")
           ;;      (or (equal key "printery")
           ;;          (equal key "faxes")))
           (yml.is-daily-product product))
          (setf result 0)
          (if (or
               (equal key "krupnaya-bitivaya-tehnika")
               (equal key "vstraivaemye-rabochie-poverhnosti")
               (equal key "vstraivaemye-rabochie-komplekti")
               (equal key "vityajki")
               (equal key "stiralnie-mashiny")
               (equal key "posudomoechnie-mashiny")
               (equal key "plity")
               (equal key "holodilniki-i-morozilniki")
               (equal key "duhovki")
               (equal key "kondicioneri")
               (let ((diagonal (get-option product "Экран" "Диагональ экрана, дюйм")))
                 (if (equal diagonal "")
                     (setf diagonal nil))
                 (setf diagonal (ceiling (parse-float diagonal)))
                 (> diagonal 51)))
              (setf result 500)
              (if (or
                   (equal key "dlya-snyatiya-katiwkov")
                   (equal key "vesi-napolnie")
                   (equal key "trimmery")
                   (equal key "shipci")
                   (equal key "gigiena-i-zdorovie")
                   (equal key "feny")
                   (equal key "epilyatory")
                   (equal key "britvy")
                   (and (equal key "elektrochainiki-i-termopoty")
                        (let ((type (get-option product "Общие характеристики" "Тип")))
                          (string-not-equal type "Термопот")))
                   (equal key "planshetnie-komputery")
                   (equal key "mobilephones")
                   (equal key "cartridge-dlya-printerov")
                   (equal key "transmittery")
                   (equal key "alarm")
                   (equal key "kamerizadnegowida")
                   (equal key "parktronics")
                   (equal key "videoregistratori")
                   (equal key "radar-detectori")
                   ;; (equal key "avtomobilnie-usiliteli")
                   (equal key "avtomobilnie-televizori")
                   (and (equal key "avtomobilnie-subvuferi")
                        (let ((type (get-option product "Общие характеристики" "Тип")))
                          (string-not-equal type "Сабвуфер")))
                   (equal key "avtomobilnie-kolonki")
                   (equal key "avtomagnitoli")
                   (equal key "radiostations")
                   (equal key "changers")
                   (equal key "cifrovye-fotoapparaty")
                   (equal key "klei")
                   (equal key "promhimiya")
                   (equal key "pilki-polotna")
                   (equal key "subilo")
                   (equal key "schlif-lenti-listi")
                   (equal key "dlya-steplerov")
                   (equal key "fresi")
                   (equal key "dlya-sadovoy-tehniki")
                   (equal key "patroni")
                   (equal key "dlya-swarki")
                   (equal key "kordschetki")
                   (equal key "swerla")
                   (equal key "krugi-schlif")
                   (equal key "krugi-pilnie")
                   (equal key "cepi-pilnie")
                   (equal key "krugi-lepestkovie")
                   (equal key "buri")
                   (equal key "biti")
                   (equal key "koronki-chaschi")
                   (equal key "drel")
                   (equal key "schtativi")
                   (equal key "multimetri")
                   (equal key "ruletka")
                   (equal key "uklonomer")
                   (equal key "nivelir")
                   (equal key "uglomeri")
                   (equal key "kurwimetr")
                   (equal key "detektori")
                   (equal key "dalnomeri")
                   (equal key "urovni")
                   (equal key "ellobsik")
                   (equal key "schlifovalnie-maschini")
                   (equal key "radiotelefoni")
                   (equal key "stactelefon")
                   (equal key "videokamery")
                   )
                  (setf result 100)))))
    result))

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


(defun yml-page-for-parser ())
  ;; (let ((yml-data))
  ;;   (setf (hunchentoot:content-type*) "application/xml; charset=utf-8")
  ;;   (setf yml-data (soy.yml:xml
  ;;                   (list :datetime (time.get-date-time)
  ;;                         :marketname "ЦиFры 320-8080"
  ;;                         :marketcompany "ЦиFры 320-8080"
  ;;                         :marketurl "http://www.320-8080.ru/"
  ;;                         :categoryes
  ;;                         (loop
  ;;                            :for key
  ;;                            :being :the hash-key
  ;;                            :in (yml-groups)
  ;;                            :when (getobj key 'group)
  ;;                            :collect (let ((obj (getobj key 'group)))
  ;;                                       (list :id (yml-id obj)
  ;;                                             :name (name obj)
  ;;                                             :parent (if (null (parent obj))
  ;;                                                         0 ; если это вершина дерева
  ;;                                                         (yml-id (parent obj))))))
  ;;                         :offers (format nil "~{~a~}"
  ;;                                         (collect-storage
  ;;                                          'product
  ;;                                          ;;продукт должен находиться в группе маркированной как ymlshow
  ;;                                          ;;быть активным и иметь не нулевую цену
  ;;                                          :when-fn #'yml.yml-show-p
  ;;                                          :func #'(lambda (product)
  ;;                                                    (soy.yml:offer (list :articul (articul product)
  ;;                                                                         :price (siteprice product)
  ;;                                                                         :category (gethash
  ;;                                                                                    (key (parent product))
  ;;                                                                                    *yml-group-ids*)
  ;;                                                                         :picture (let ((pics (get-pics
  ;;                                                                                               (articul product))))
  ;;                                                                                    (when pics
  ;;                                                                                      (encode-uri (car pics))))
  ;;                                                                         :name (let ((yml-name (get-option
  ;;                                                                                                product "Secret" "Yandex"))
  ;;                                                                                     (parser-name (get-option
  ;;                                                                                                   product "Secret" "Parser")))
  ;;                                                                                 (if (valid-string-p parser-name)
  ;;                                                                                     parser-name
  ;;                                                                                     (if (or (null yml-name)
  ;;                                                                                             (string= ""
  ;;                                                                                                      (stripper yml-name))
  ;;                                                                                             (string= "No"
  ;;                                                                                                      (stripper yml-name)))
  ;;                                                                                         (name product)
  ;;                                                                                         yml-name)))
  ;;                                                        :description nil))))))
  ;;    ;; (let ((raw (list-raw-data dump)))
  ;;    ;;    (bt:make-thread #L(gateway.%store-and-processed-dump raw)
  ;;    ;;                    :name "store-and-processed-dump"))

  ;;  ))


(defun make-yml-categoryes()
  (loop
     :for key
     :being :the hash-key
     :using (hash-value id)
     :in (yml-groups)
     :when (getobj key 'group)
     :collect (let ((obj (getobj key 'group)))
                (list :id (yml-id obj)
                      :name (name obj)
                      :parent (if (null (parent obj))
                                  0 ; если это вершина дерева
                                  (yml-id (parent obj)))))))

(defun make-yml-offers()
  (collect-storage
   'product
   ;;продукт должен находиться в группе маркированной как ymlshow
   ;;быть активным и иметь не нулевую цену
   :when-fn #'yml.yml-show-p
   :func #'(lambda (product)
             (soy.yml:offer (list :articul (articul product)
                                  :deliveryprice (yml.get-product-delivery-price1 product)
                                  :price (siteprice product)
                                  :category (gethash
                                             (key (parent product))
                                             *yml-group-ids*)
                                  :picture  (let ((pics (get-pics
                                                         (articul product))))
                                              (when pics
                                                (encode-uri (car pics))))
                                  :name   (let ((yml-name (get-option product "Secret" "Yandex")))
                                            (if (or (null yml-name)
                                                    (string= ""
                                                             (stripper yml-name))
                                                    (string= "No"
                                                             (stripper yml-name)))
                                                (name-seo product)
                                                yml-name))
                                  :description nil)))))


(defun make-yml-data()
  (soy.yml:xml
   (list :datetime (time.get-date-time)
         :marketname "ЦиFры 320-8080"
         :marketcompany "ЦиFры 320-8080"
         :marketurl "http://www.320-8080.ru/"
         :categoryes (make-yml-categoryes)
         :offers (format nil "~{~a~}" (make-yml-offers)))))


(defun create-yml-file ()
  (let ((filename (merge-pathnames "yml.xml" (config.get-option :critical :path-to-conf))))
    (with-open-file
        (stream filename :direction :output :if-exists :supersede)
      (format stream "~a" (make-yml-data)))))

(defun yml.test-groups (group)
  (mapcar #'(lambda (v)
              (if (groups v)
                  (yml.test-groups v)
                  (format t "~& (equal key \"~a\")" (key v))))
          (groups group)))


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
