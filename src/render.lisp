;;;; render.lisp

(in-package #:eshop)


(defclass eshop-render () ())

(defun render.breadcrumbs (in &optional out)
  "Processing parents until nil, creating breadcrumbs"
  (if in
      (progn
        (if (productp in)
            (push (list :key (articul in) :val (name-seo in)) out)
            (push (list :key (key in) :val (name in)) out))
        (render.breadcrumbs (parent in) out))
      ;; else -  end of recursion
      (list :breadcrumbelts (butlast out)
            :breadcrumbtail (car (last out)))))

(defun menu-sort (a b)
  "Function for sorting groups by order field"
  (when (and (order a) (order b))
    (< (order a)
       (order b))))

;;TODO временно убрана проверка на пустые группы, тк это поле невалидно
(defun render.menu (&optional current-object (show-all nil))
  "Creating left menu"
  (let* ((current-root (get-root-parent current-object))
         (dividers (list "setevoe-oborudovanie" "foto-and-video" "rashodnye-materialy"))
         (src-lst
          (mapcar #'(lambda (val)
                      (if (and current-root
                               (equal (key val) (key current-root)))
                          ;; This is current
                          (soy.menu:selected
                           (list :divider (member (key val) dividers :test #'equal)
                                 :key (key val)
                                 :name (name val)
                                 :icon (icon val)
                                 :subs (loop
                                          :for child
                                          :in (sort
                                               (remove-if #'(lambda (g)
                                                              (or
                                                               (zerop (storage.count-recursive-products g (if show-all
                                                                                                              #'identity
                                                                                                              #'active)))
                                                               (not (active g))))
                                                          (groups val))
                                               #'menu-sort)
                                          :collect
                                          (list :key (key child) :name (name child)))))
                          ;; else - this is ordinal
                          (soy.menu:ordinal (list :divider (member (key val) dividers :test #'equal)
                                                  :key  (key val)
                                                  :name (name val)
                                                  :icon (icon val)))))
                  (remove-if-not #'active (get-root-groups)))))
    (soy.menu:main (list :elts src-lst))))

;;; KEYWORDS

(defmethod render.get-keywords ((object group) &optional (parameters (request-get-plist)))
  (declare (ignore parameters))
  (let* ((h1 (name object))
         (vendor (hunchentoot:get-parameter "vendor")))
    (when vendor
      (setf h1 (concatenate 'string h1 " " (vendor-transform-from-alias vendor))))
    (format nil "~A купить цена каталог интернет-магазин характеристики описание фото отзывы гарантия доставка"
            h1)))

(defmethod render.get-keywords ((object product) &optional (parameters (request-get-plist)))
  (declare (ignore parameters))
  (let ((page-name (name-seo object)))
    (format nil "~a, ~a купить, ~a цена, ~a в Санкт-Петербурге, ~a продажа"
            page-name page-name page-name page-name  page-name)))

(defmethod render.get-keywords ((object filter) &optional (parameters (request-get-plist)))
  (declare (ignore parameters))
  (let ((fltr-name (getf (data object) :name)))
    (format nil "~A купить цена каталог интернет-магазин характеристики описание фото отзывы гарантия доставка"
            fltr-name)))
;;; TITLE

(defmethod render.get-title ((object group) &optional (parameters (request-get-plist)))
  (declare (ignore parameters))
  (let ((h1 (name object))
        (page (hunchentoot:get-parameter "page"))
        (vendor (hunchentoot:get-parameter "vendor")))
    (when vendor
      (setf h1 (concatenate 'string h1 " " (vendor-transform-from-alias vendor))))
    (format nil "~A – купить по выгодным ценам | ~A – каталог в интернет-магазине «ЦиFры»:
 характеристики, описание, фото, отзывы~A"
            h1 h1
            (if (and page
                     (string< "1" page))
                (format nil " | Страница ~A" page)
                ""))))

(defmethod render.get-title ((object product) &optional (parameters (request-get-plist)))
  (declare (ignore parameters))
  (format nil "~A Арт.:~A | ~A | Купить по выгодной цене в интернет-магазине «ЦыFры» | ~A: обзор, характеристики, фото, описание, отзывы"
          (name-seo object)
          (articul object)
          (aif (parent object)
               (name it)
               "")
          (name-seo object)
          ))

(defmethod render.get-title ((object filter) &optional (parameters (request-get-plist)))
  (declare (ignore parameters))
  (let ((fltr-name (getf (data object) :name))
         (group-name (name (parent object)))
         (page (hunchentoot:get-parameter "page")))
    (format nil "~A | ~A | купить по выгодным ценам в интернет-магазине «ЦиFры»:
 характеристики, описание, фото, отзывы~A"
            group-name fltr-name
            (if (and page
                     (string< "1" page))
                (format nil " | Страница ~A" page)
                ""))))

;;; DESCRIPTION

(defmethod render.get-description ((object group) &optional (parameters (request-get-plist)))
  (declare (ignore parameters))
  (let ((h1 (name object))
        (vendor (hunchentoot:get-parameter "vendor")))
    (when vendor
        (setf h1 (concatenate 'string h1 " " (vendor-transform-from-alias vendor))))
    (format nil "~A по выгодным ценам в интернет-магазине «ЦиFры». В каталоге вы можете ознакомиться с ценами, отзывами покупателей, описанием, фотографиями и подробными техническими характеристиками товаров. У нас можно купить ~A с гарантией и доставкой."
             h1
             h1)))


(defmethod render.get-description ((object product) &optional (parameters (request-get-plist)))
   (declare (ignore parameters))
   (format nil "~A по выгодной цене в интернет-магазине «ЦиFры». В каталоге вы можете ознакомиться с ценами, отзывами покупателей, описанием, фотографиями и подробными техническими характеристиками товаров. У нас можно купить ~A с гарантией и доставкой."
           (name-seo object)
           (name-seo object)))

(defmethod render.get-description ((object filter) &optional (parameters (request-get-plist)))
  (declare (ignore parameters))
  (let ((fltr-name (getf (data object) :name)))
    (format nil "~A по выгодным ценам в интернет-магазине «ЦиFры». В каталоге вы можете ознакомиться с ценами, отзывами покупателей, описанием, фотографиями и подробными техническими характеристиками товаров. У нас можно купить ~A с гарантией и доставкой."
            fltr-name fltr-name)))



(defmethod render.render ((object group) &optional (parameters (request-get-plist)))
  (let ((name (name object))
        (showall (getf parameters :showall)))
    (default-page
        (soy.catalog:content
         (list :name name
               :breadcrumbs (soy.catalog:breadcrumbs (breadcrumbs-add-vendor1 (render.breadcrumbs object) parameters))
               :menu (render.menu object)
               :rightblocks (append
                             (marketing-filters.render-filters object showall)
                             ;;fullfilter
                             (let ((ret (rightblocks object parameters)))
                               (awhen (getobj (string-downcase (getf parameters :vendor)) 'vendor)
                                 (push (marketing-filters.render-vendor-filters object it showall) ret))
                               (when (fullfilter object)
                                 (push (render.render (fullfilter object) parameters) ret))
                               ret))
               :subcontent  (if (and (null (products object))
                                     (null (getf parameters :fullfilter))
                                     (null (getf parameters :vendor)))
                                ;; Отображаем группы
                                (soy.catalog:centergroup
                                 (list
                                  :producers (when showall
                                               (render.show-producers (storage.get-filtered-products object #'atom)))
                                  :accessories (soy.catalog:accessories)
                                  :groups (let ((sort-groups (sort (remove-if-not #'(lambda (group)
                                                                                      (and (active group)
                                                                                           (plusp (storage.count-recursive-products group (if showall
                                                                                                                                              #'identity
                                                                                                                                              #'active)))))
                                                                                  (groups object)) #'menu-sort)))
                                            (mapcar #'(lambda (child)
                                                        (let* ((show-func (if showall
                                                                              #'atom
                                                                              #'active))
                                                               (products (storage.get-filtered-products child show-func))
                                                               (filters (marketing-filters.get-filters child products)))
                                                          (list
                                                           :is-active (active child)
                                                           :name (name child)
                                                           :key (key child)
                                                           :cnt (if products
                                                                    (length products)
                                                                    "-")
                                                           :pic (pic child)
                                                           :filters (mapcar #'(lambda (v)
                                                                                (let ((filter (car v))
                                                                                      (num (cdr v)))
                                                                                  (list :name (name filter)
                                                                                        :groupkey (key child)
                                                                                        :key (key filter)
                                                                                        :num (format nil "(~a)" num))))
                                                                            filters))))
                                                    sort-groups))))
                                ;;else
                                (let* ((products-list (if showall
                                                          (storage.get-filtered-products object #'atom)
                                                          (storage.get-filtered-products object #'active)))
                                       (cartrige-group (string= "cartridge-dlya-printerov" (key object)))
                                       (printer-articul (and cartrige-group (getf parameters :printer-articul)))
                                       (storage-printer (when printer-articul
                                                          (getobj printer-articul 'product)))
                                       (cartrige-printer (when printer-articul
                                                           (gethash printer-articul *printer-storage*))))
                                  (unless (getf parameters :sort)
                                    (setf (getf parameters :sort) "pt"))
                                  (awhen (getf parameters :vendor)
                                    (setf products-list
                                          (remove-if-not
                                           #'(lambda (p)
                                               (vendor-filter-controller p (vendor-transform-from-alias (string-downcase it))))
                                           products-list)))
                                  (when (getf parameters :fullfilter)
                                    (setf products-list (fullfilter-controller products-list object parameters)))
                                  (when (and printer-articul cartrige-printer)
                                    (setf products-list (keys-to-objects
                                                         (cartrige.get-cartriges-by-model printer-articul)
                                                         :type 'product)))
                                  (with-sorted-paginator
                                      products-list
                                    parameters
                                    (soy.catalog:centerproduct
                                     (list
                                      :sorts (sorts parameters)
                                      :producers (render.show-producers
                                                  (storage.get-filtered-products
                                                   object
                                                   (if showall #'atom #'active)))
                                      :accessories (soy.catalog:accessories)
                                      :pager pager
                                      :cartrigeselect (when cartrige-group
                                                        (soy.catalog:cartige-select
                                                         (list
                                                          :vendors (cartrige.get-vendors-list)
                                                          :printer (when (and storage-printer
                                                                              cartrige-printer)
                                                                     (soy.catalog:printer-view (list
                                                                                                :articul printer-articul
                                                                                                :pic (car (get-pics (format nil "~A" printer-articul)))
                                                                                                :name (name-seo storage-printer)
                                                                                                :key (key storage-printer)
                                                                                                :price (if (active storage-printer)
                                                                                                           (siteprice storage-printer))))))))
                                      :products
                                      (loop
                                         :for product :in  paginated :collect (render.view product)))))))))
        :line-banner (awhen (getobj (string-downcase (getf parameters :vendor)) 'vendor)
                       (when (equal (name it) "Nokia")
                         (list :URL "/articles/nokia_bonus?bannerType=line-text"
                               :SRC "/img/banners/nokia.png"
                               :NAME "Узнай специальную цену для членов F-клуба!"
                               :SRC2 "/img/banners/nokia.png")))
        :keywords (render.get-keywords object parameters)
        :description (render.get-description object parameters)
        :title (render.get-title object parameters))))

(defun render.get-range-limits (group optgroup-name option-name)
  ())

(defmethod render.render ((object group-filter) &optional (parameters (request-get-plist)))
  (when (not (equal "" object))
    (soy.fullfilter:container
     (list :name (name object)
           :vendor (getf parameters :vendor)
           :sort (getf parameters :sort)
           :base (format nil "~{~a~}"
                         (mapcar #'(lambda (elt)
                                     (filter-element elt parameters))
                                 (base object)))
           :advanced (when (advanced object)
                       (format nil "~{~a~}"
                               (mapcar #'(lambda (elt)
                                           (soy.fullfilter:group
                                            (list :name (car elt)
                                                  :elts (mapcar #'(lambda (inelt)
                                                                    (filter-element inelt parameters))
                                                                (cadr elt)))))
                                       (advanced object))))
           :isshowadvanced (is-need-to-show-advanced object parameters)))))

(defmethod restas:render-object ((designer eshop-render) (object group-filter))
  (render.render object))

(defun render.gen-uspale-product (prod)
  (when prod
    (let* ((articul (articul prod))
           (name (name-seo prod))
           (group (parent prod))
           (pic (car (get-pics (key prod))))
           (siteprice (siteprice prod)))
      (list
       ;;TODO: :rating
       ;;TODO: :badge
       :grouplink (key group)
       :groupname (name group)
       :articul articul
       :pic pic
       :name name
       :showsiteprice (get-format-price (siteprice prod))
       :showprice (get-format-price (price prod))
       :buttonaddcart (soy.buttons:add-product-cart (list
                                                     :articul articul
                                                     :name name
                                                     :pic pic
                                                     :siteprice siteprice))))))

(defun render.prepare-upsale-block (name items)
  (list :name name
        :items (mapcar (lambda (item)
                         (render.gen-uspale-product item))
                       items)))


(defun render.get-upsale-products (obj)
  (cond
    ((groupp obj) (get-randoms-from-list (remove-if-not #'active (products obj)) 4))
    ((filterp obj) (get-randoms-from-list (remove-if-not #'active
                                                         (filters.filter obj :obj-set (products (parent obj)))) 4))
    (t nil)))

(defmethod render.prepare-upsale-full ((object group))
  (list :groupnameskl (sklonenie (name object) 2)
        :upsaleblocks (remove-if #'null
                       (mapcar #'(lambda (obj)
                                   (cond
                                     ((groupp obj) (render.prepare-upsale-block (name obj)
                                                                                (render.get-upsale-products obj)))
                                     ((filterp obj) (render.prepare-upsale-block (getf (data obj) :name)
                                                                                (render.get-upsale-products obj)))
                                     (t nil)))
                              (upsale-links object))
                       )))


(defun render.%add-button (product)
  (declare (product product))
  (let ((fields (list :articul (articul product)
                      :name (name-seo product)
                      :pic (first (get-pics product))
                      :deliveryprice (get-product-delivery-price product)
                      :siteprice (siteprice product)
                      :price (price product))))
    (if (active product)
        (if (preorder product)
            (soy.buttons:buy-product-preorder fields)
            (soy.buttons:add-product-cart fields))
        ;;else (not active)
        (when (yml.available-for-order-p product)
          (soy.buttons:buy-product-order fields)))))

(defun %get-yml-name (product)
  (declare (product product))
  (let ((yml-name (get-option product "Secret" "Yandex")))
    (if (or (null yml-name)
            (string= ""
                     (stripper yml-name))
            (string= "No"
                     (stripper yml-name)))
        (name-seo product)
        yml-name)))

(defun render.get-special-yml-name (product)
  (declare (product product))
  (awhen (parent product)
    (when (or (find (key it) '("noutbuki"
                               "netbuki") :test #'equal)
              (and (equal (key it) "planshetnie-komputery")
                   (find (vendor product)
                         '("Apple" "Samsung") :test #'equal)))
      (%get-yml-name product))))


(defmethod render.view ((object product))
  (let ((pics (get-pics (key object))))
    (let ((group (parent object)))
      (list :articul (articul object)
            :name (name-seo object)
            :groupname (if (null group)
                           "group not found"
                           (name group))
            :groupkey  (if (null group)
                           ""
                           (key group))
            :price (siteprice object)
            :bonuscount (when (and (bonuscount object)
                                   (plusp (bonuscount object)))
                          (* 10 (bonuscount object)))
            :formatprice (get-format-price (siteprice object))
            :formatstoreprice (get-format-price
                               (+ (siteprice object)
                                  (delta-price object)))
            :bestprice (plusp (delta-price object))
            :groupd (groupd.is-groupd object)
            ;;TODO
            :freedelivery (zerop (get-product-delivery-price object))
            :groupd_holiday (groupd.holiday.is-groupd object)
            :firstpic (car pics)
            :promotiontext (concatenate 'string
                                        (get-option object "Secret" "Продающий текст")
                                        " "
                                        (if (zerop (get-product-delivery-price object))
                                            " Бесплатная доставка при заказе прямо сейчас!"
                                            (if (= 100 (get-product-delivery-price object))
                                                " Акция: доставка по городу 100 рублей!"
                                                (if (= 200 (get-product-delivery-price object))
                                                    " Акция: скидка на доставку 30%. Закажи сейчас!"
                                                    (if (= 400 (get-product-delivery-price object))
                                                        " Акция: скидка на доставку 20. Закажи сейчас!")))))
            :keyopts (render.get-catalog-keyoptions object)
            :ymlname (aif (get-option object "Общие характеристики" "Код производителя")
                         (if (string= "" it)
                             nil
                             it)
                            nil)
            :oneclickbutton  (unless (preorder object)
                               (soy.buttons:add-one-click (list :articul (articul object))))
            :addbutton (render.%add-button object)))))


(defun render.render-optgroups (optgroups)
  (let ((optlist
         (remove-if
          #'null
          (mapcar
           #'(lambda (optgroup)
               ;;не отображать группу опций с именем "Secret"
               (if (string/= (getf optgroup :name)
                             "Secret")
                   (let ((options
                          (mapcar #'(lambda (option)
                                      (unless (equal "" (getf option :value))
                                        (soy.product:option
                                         (list :name (getf option :name)
                                               :value (getf option :value)))))
                                  (getf optgroup :options))))
                     (if (notevery #'null options)
                         (soy.product:optgroup (list :name (getf optgroup :name)
                                                     :options options))
                         ""))))
          optgroups))))
    (when optlist
      (soy.product:optlist (list :optgroups optlist)))))

(defmethod render.get-catalog-keyoptions ((object product))
  (let ((parent (parent object)))
    (when parent
      (remove-if
       #'null
       (mapcar
        #'(lambda (pair)
            (let ((key-optgroup (getf pair :optgroup))
                  (key-optname  (getf pair :optname))
                  (key-alias  (getf pair :showname))
                  (key-units  (getf pair :units))
                  (optvalue))
              (mapcar #'(lambda (optgroup)
                          (when (string= (getf optgroup :name) key-optgroup)
                            (let ((options (getf optgroup :options)))
                              (mapcar #'(lambda (option)
                                          (if (string= (getf option :name) key-optname)
                                              (setf optvalue (getf option :value))))
                                                   options))))
                      (optgroups object))
              (when (valid-string-p optvalue)
                (list :optgroup key-alias
                      :optvalue (if (equal "Есть" optvalue)
                                    ""
                                    optvalue)
                      :optunits key-units))))
        (catalog-keyoptions parent))))))



(defun render.get-keyoptions (product)
  (declare (product product))
  (let ((parent (parent product)))
    (when parent
      (mapcar #'(lambda (pair)
                  (let* ((key-optgroup (getf pair :optgroup))
                         (key-optname  (getf pair :optname))
                         (key-alias  (getf pair :showname))
                         (key-units  (getf pair :units))
                         (optvalue (get-option product key-optgroup key-optname)))
                    (list :optgroup key-optgroup
                          :optname (if (valid-string-p key-alias)
                                       key-alias
                                       key-optname)
                          :optvalue optvalue
                          :optunits key-units)))
              (keyoptions parent)))))

(defmethod render.relink ((object product))
  (let ((temp-rs1)
        (temp-rs2))
    ;;2 случайных товара из списка
    (setf temp-rs1 (get-randoms-from-list
                    ;; список активных товаров той же группы и того же производителя
                    ;; кроме его самого
                    (let ((base-vendor (vendor object)))
                      (remove-if-not
                       #'(lambda (x)
                           (and (not (equal x object))
                                (active x)
                                (equal (vendor object) base-vendor)))
                       (storage.get-filtered-products (parent object))))
                    2))
    ;;4 случайных товара из списка
    (setf temp-rs2 (get-randoms-from-list
                    ;;список всех активных товаров кроме object
                    (let ((all))
                      (process-storage #'(lambda (v)
                                           (when (and (active v)
                                                      (not (equal v object)))
                                             (push v all)))
                                       'product)
                      all)
                    4))
    (loop
       :for item in (append temp-rs1 temp-rs2)
       :for n
       :from 1 to 4
       :collect item)))

(defmethod restas:render-object ((designer eshop-render) (object product))
  (aif (get-option object "Secret" "Дубль")
       (hunchentoot:redirect (concatenate 'string "/" it) :code 301))
  (let* ((pics (get-pics (key object)))
         (diff-percent (servo.diff-percentage (price object) (siteprice object)))
         (is-available (yml.available-for-order-p object))
         (is-vintage (not (or (active object) is-available)))
         (product-view)
         (group (parent object)))
    (setf product-view (list :menu (render.menu object)
                             :breadcrumbs (soy.product:breadcrumbs (render.breadcrumbs object))
                             :articul (articul object)
                             :name (name-seo object)
                             :ymlname (aif (get-option object
                                                       "Общие характеристики"
                                                       "Код производителя")
                                           (if (string= "" it)
                                               nil
                                               it)
                                           nil)
                             :siteprice (siteprice object)
                             :storeprice (price object)
                             :bestprice (plusp (delta-price object))
                             :freedelivery (zerop (get-product-delivery-price object))
                             :groupd (groupd.is-groupd object)
                             :groupd_holiday (groupd.holiday.is-groupd object)
                             :bonuscount (when (and (bonuscount object)
                                                    (plusp (bonuscount object)))
                                           (* 5 (bonuscount object)))
                             :formatsiteprice  (if (= 0 (siteprice object))
                                                     nil
                                                     (get-format-price (siteprice object)))
                             :formatstoreprice (get-format-price
                                                (+ (siteprice object)
                                                   (delta-price object)))
                             :equalprice (zerop (delta-price object))
                             :diffprice (delta-price object)
                             :upsaleinfo (when (and group (upsale-links group))
                                           (soy.product:upsale (render.prepare-upsale-full group)))
                             :procent diff-percent
                             :subst (format nil "/~a" (articul object))
                             :pics (cdr pics)
                             :firstpic (when pics (car pics))
                             :optlist (render.render-optgroups (optgroups object))
                             :slogan (concatenate 'string
                                                  (get-option object "Secret" "Продающий текст")
                                                  " "
                                                  (if (zerop (get-product-delivery-price object))
                                                      " Бесплатная доставка при заказе прямо сейчас!"
                                                      (if (= 100 (get-product-delivery-price object))
                                                          " Акция: доставка по городу 100 рублей!"
                                                          (if (= 200 (get-product-delivery-price object))
                                                              " Акция: скидка на доставку 30%. Закажи сейчас!"
                                                              (if (= 400 (get-product-delivery-price object))
                                                                  " Акция: скидка на доставку 20%. Закажи сейчас!")))))
                             :others (soy.product:others
                                      (list :others (mapcar #'(lambda (x)
                                                                (if (productp x)
                                                                    (render.view x)
                                                                    (list :aricul "0"
                                                                          :name ""
                                                                          :pic "/img/temp/i6.jpg"
                                                                          :price "0"
                                                                          :siteprice "0" :subst ""
                                                                          :firstpic "/img/temp/i6.jpg")))
                                                            (render.relink object))))
                             :keyoptions (filters.limit-end
                                          (remove-if-not #'(lambda (v)
                                                             (valid-string-p
                                                              (getf v :optvalue)))
                                                         (render.get-keyoptions object))
                                          6)
                             :active (or (active object)
                                         (yml.available-for-order-p object))
                             :vintage is-vintage
                             :shortdescr (seo-text object)
                             :bestproducts (soy.product:similar-products
                                            (list :products (mapcar #'(lambda (prod)
                                                                        (soy.catalog:product
                                                                         (render.view prod)))
                                                                    (filters.limit-end (servo.find-relative-product-list object) 4))))
                             :seotextflag (valid-string-p (seo-text object))
                             :predzakaz (or (preorder object)
                                            (yml.available-for-order-p object))
                             :addproductcart (render.%add-button object)
                             :isavaiblefororder is-available
                             :addoneclick (unless (or (preorder object)
                                                      (yml.available-for-order-p object))
                                            (soy.buttons:add-one-click (list :articul (articul object)
                                                                             :available is-available)))))
    (unless (groupd.is-groupd object)
      (aif (getf product-view :bonuscount)
           (setf it (* 2 it))))
    (default-page
        (soy.product:content product-view)
        :line-banner (when (and group
                                (equal (key group) "mobilephones")
                                (equal (vendor object) "Nokia"))
                       (list :URL "/articles/nokia_bonus?bannerType=line-text"
                             :SRC "/img/banners/nokia.png"
                             :NAME "Узнай специальную цену для членов F-клуба!"
                             :SRC2 "/img/banners/nokia.png"))
        :keywords (render.get-keywords object nil)
        :description (render.get-description object nil)
        :title (render.get-title object nil))))


(defun render.make-producters-reverse-lists-by-column (list  &optional (columns 4))
  (let ((len (length list))
        (rs)
        (start-pos 0)
        (segment))
    (multiple-value-bind (division remainder)
        (truncate len columns)
      (loop
         :for i :from 1 :to columns
         :do (progn
               (setf segment division)
               (when (plusp remainder)
                 (setf segment (1+ segment))
                 (setf remainder (1- remainder)))
               (push (subseq list start-pos (+ start-pos segment)) rs)
               (setf start-pos (+ start-pos segment)))))
    rs))

(defun render.make-producters-base (list  &key (columns 4) cut)
  (let ((reverse-lists (render.make-producters-reverse-lists-by-column list columns))
        (rs))
    (mapcar #'(lambda (v)
                (let ((len cut))
                  (if (or (null cut)
                          (< (length v) cut))
                      (setf len (length v)))
                  (push (subseq v 0 len) rs)))
            reverse-lists)
    (remove-if #'null rs)))

(defun render.make-producters-hidden (list  &key (columns 4) cut)
  (let ((reverse-lists (render.make-producters-reverse-lists-by-column list columns))
        (rs))
    (mapcar #'(lambda (v)
                (let ((len cut))
                  (if (or (null cut)
                          (< (length v) cut))
                      (setf len (length v)))
                  (push (subseq v len) rs)))
            reverse-lists)
    (remove-if #'null rs)))


(defmethod restas:render-object ((designer eshop-render) (object filter))
  (let* ((request-get-plist (request-get-plist))
         (fltr-name  (getf (data object) :name))
         (group (parent object))
         (group-name (name group))
         (showall (getf request-get-plist :showall))
         (vendor (tbnl:parameter "vendor"))
         (group-children (marketing-filters.group-children group showall))
         (products-list (filters.filter object :obj-set group-children)))
    (if (null (getf request-get-plist :sort))
        (setf (getf request-get-plist :sort) "pt"))
    (awhen (getf (request-get-plist) :vendor)
      (setf products-list
            (remove-if-not #'(lambda (p)
                               (vendor-filter-controller p it))
                           products-list)))
    (with-sorted-paginator
        products-list
      request-get-plist
      (default-page
          (soy.catalog:content
           (list :name fltr-name
                 :breadcrumbs (soy.catalog:breadcrumbs (render.breadcrumbs object))
                 :menu (render.menu object)
                 :rightblocks (append
                               (marketing-filters.render-filters group showall)
                               (awhen (getobj (string-downcase vendor) 'vendor)
                                 (list (marketing-filters.render-vendor-filters group it showall)))
                               (rightblocks object request-get-plist))
                 :subcontent (soy.catalog:centerproduct
                              (list
                               :sorts (sorts request-get-plist)
                               :producers (render.show-producers group-children)
                               :accessories (soy.catalog:accessories)
                               :pager pager
                               :products (loop
                                            :for product
                                            :in  paginated
                                            :collect (render.view product))))))
          :keywords (render.get-keywords object nil)
          :description (render.get-description object nil)
          :title (render.get-title object nil)))))



(defun render.show-producers (products)
  (declare (list products))
  (let* ((vendors (storage.get-vendors products))
         (url-parameters (request-get-plist))
         (veiws nil))
    (remf url-parameters :page)
    (maphash #'(lambda (k x)
                 (let* ((vendor-alias (awhen (getobj (string-downcase k) 'vendor)
                                             (alias it)))
                        (vendor-url (if (valid-string-p vendor-alias)
                                        vendor-alias
                                        k)))
                   (setf (getf url-parameters :vendor) (hunchentoot:url-encode vendor-url))
                   (push (list :vendor k
                               :cnt x
                               :link (format nil "?~a" (servo.make-get-str url-parameters)))
                         veiws)))
             vendors)
    (setf veiws (sort veiws #'string<= :key #'(lambda (v) (string-upcase (getf v :vendor)))))
    (soy.catalog:producers
     (list
      :vendorblocks (render.make-producters-base
                     veiws :cut 6)
      :vendorhiddenblocks (render.make-producters-hidden
                           veiws :cut 6)))))


(defmethod restas:render-object ((designer eshop-render) (object group))
  (render.render object))
