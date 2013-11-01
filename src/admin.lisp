;;;; admin.lisp

(in-package #:eshop)

(restas:define-route admin-route ("/administration-super-panel")
  (:decorators (@protected "admin" "content" "tks"))
  (restas:redirect 'admin/-route))

(restas:define-route admin/-route ("/administration-super-panel/")
  (:decorators (@protected "admin" "content" "tks"))
  (show-admin-page "info"))

(restas:define-route admin-actions-key-route ("/administration-super-panel/actions" :method :post)
  (:decorators (@protected "admin" "content"))
  (show-admin-page "actions"))

(restas:define-route admin-pics-route ("/administration-super-panel/pics" :method :post)
  (:decorators (@protected "admin" "content"))
  (show-admin-page "pics"))

(restas:define-route admin-templates-route ("/administration-super-panel/templates" :method :post)
  (:decorators (@protected "admin" "content"))
  (show-admin-page "templates"))

(restas:define-route admin-backup1-route ("/administration-super-panel/makebackup")
  (:decorators (@protected "admin"))
  (show-admin-page "backup"))

(restas:define-route admin-backup-route ("/administration-super-panel/makebackup" :method :post)
  (:decorators (@protected "admin"))
  (show-admin-page "backup"))

;; (restas:define-route admin-black-list-route ("/administration-super-panel/black-list")
;;   (show-admin-page "black-list"))

(restas:define-route admin-black-list-post-route ("/administration-super-panel/black-list" :method :post)
  (:decorators (@protected "tks" "content"))
  (show-admin-page "black-list"))

(restas:define-route admin-cron-route ("/administration-super-panel/cron-jobs" :method :post)
  (:decorators (@protected "admin"))
  (show-admin-page "cron-jobs"))

(restas:define-route admin-parenting-key-route ("/administration-super-panel/parenting" :method :post)
  (:decorators (@protected "admin" "content"))
  (show-admin-page "parenting"))

(restas:define-route admin-vendor-seo-route ("/administration-super-panel/vendor-seo" :method :post)
  (:decorators (@protected "admin"))
  (show-admin-page "vendor-seo"))

(restas:define-route admin-key-route ("/administration-super-panel/:key")
  (:decorators (@protected "admin" "content"))
  (show-admin-page key))

(defun admin.standard-ajax-response (success &optional msg)
  "Standard json-encoded ajax response, include success and msg fields"
  (write-plist-to-json (list :success success :msg (if msg msg "Success"))))

(defun admin.page-wrapper (content)
  "Standard wrapper with styles, scripts, header, etc"
  (soy.admin:main
   (list :content content)))

(restas:define-route admin-black-list-route ("/administration-super-panel/black-list")
  (:decorators (@protected "tks" "content"))
  (show-admin-page "black-list"))

(restas:define-route admin-filter-create ("administration-super-panel/filter-create" :method :get)
  (:decorators (@protected "admin"))
  (switch ((hunchentoot:get-parameter "get") :test #'string=)
    ("filter-types"
     ;; FIXME: use json encode, not format
     (format nil "[~{~A~^,~}]" (mapcar #'write-plist-to-json
                                       (filters.get-basics-types))))
    ("fields"
     (format nil "[~{~A~^,~}]" (mapcar #'write-plist-to-json
                                       (filters.get-basic-fields (hunchentoot:get-parameter "filter-type")))))))

(restas:define-route admin-edit-slot-route ("administration-super-panel/edit-slot" :method :post)
  (:decorators (@protected "admin" "content"))
  (let ((object (getobj (hunchentoot:post-parameter "key")))
        (slot (anything-to-symbol (hunchentoot:post-parameter "slot")))
        (value (hunchentoot:post-parameter "value")))
    (if object
        (handler-case
            (progn
              (setf (slot-value object slot)
                    (slots.%get-data (slot-type (type-of object) slot) value))
              ;; TODO: write slot-specific fixers (which return whether whole-object fix is needed)
              ;; FIXME: bad code
              (when (and (groupp object) (equal slot 'raw-fullfilter))
                (setf (fullfilter object) (decode-fullfilter (raw-fullfilter object))))
              (when (equal slot 'parents)
                (typecase object
                  (product
                   (class-core.unbind-product-from-all-groups object)
                   (mapcar #'(lambda (group) (class-core.bind-product-to-group object group)) (parents object)))
                  (group
                   (class-core.unbind-group-from-all-parents object)
                   (mapcar #'(lambda (parent) (class-core.bind-group-to-parent object parent)) (parents object)))))
              ;; return value
              (admin.standard-ajax-response t))
          (error (e) (admin.standard-ajax-response nil (format nil "Error: ~A" e))))
        ;; else
        (admin.standard-ajax-response nil "Error: Object doesn't exist"))))



(defun admin-compile-templates ()
  (servo.compile-soy "admin.soy"
                     "class_forms.soy"))

(defun admin-update ()
  "Updates templates"
  (admin-compile-templates))

(defun admin.get-info ()
  ;; (setf (bt:thread-name (bt:current-thread)) "test")
  (list (format nil "~A<br>~A~{~{~@[~A~]~^: ~}<br>~}~A~{~A~^<br>~}"
                (concatenate 'string "<b>Последняя выгрузка: "
                                     (time.encode.backup (date *gateway.loaded-dump*))
                                     " || продуктов: "
                                     (write-to-string (product-num *gateway.loaded-dump*))
                                     " || активных: "
                                     (write-to-string (count-storage 'product :when-fn #'active))
                                     " || выключенных: "
                                     (write-to-string (hash-table-count black-list.*storage*))
                                     "</b>")
                "<br><b>Потоки</b>:<br>"
                (mapcar #'(lambda (v)
                            (let ((thread-name (bt:thread-name v)))
                              (list thread-name)))
                        (sb-thread:list-all-threads))
                "<br><b>Количество объектов в базе</b>:<br>"
                (mapcar #'(lambda (class)
                            (format nil "    ~A: ~A"
                                    class (eshop.odm:instance-count class)))
                        (mapcar #'class-name (eshop.odm:list-persistent-classes))))
        "<br><b>Память</b>:"
        (regex-replace-all "\\n" (with-output-to-string (*standard-output*) (room)) "<br>")))


(restas:define-route admin-make-get-route ("/administration-super-panel/make" :method :get)
  (:decorators (@protected "admin"))
  (admin.page-wrapper
   (let ((key (hunchentoot:get-parameter "key"))
         (type (hunchentoot:get-parameter "type")))
     (if (getobj key)
         (restas:redirect 'admin-edit-get-route)
         ;; else
         (if (and (valid-string-p type)
                  (class-exist-p (anything-to-symbol type)))
             (soy.class_forms:formwindow
              (list :key key
                    :type type
                    :fields (class-core.make-fields (get-instance type))
                    :target "make"))
             ;; else
             "Incorrect type or no type specified")))))

(restas:define-route admin-make-choose-key-route ("/administration-super-panel/make-key" :method :get)
  (:decorators (@protected "admin"))
  (admin.page-wrapper
   (soy.class_forms:make-choose-key)))

(restas:define-route admin-new-make-post-route ("/administration-super-panel/new-make" :method :post)
  (:decorators (@protected "admin"))
  (log:debug (hunchentoot:post-parameters*))
  (let ((type (string-downcase (hunchentoot:post-parameter "type")))
        (key (hunchentoot:post-parameter "key")))
    (if (getobj key)
        (restas:redirect 'admin-edit-get-route :type type :key key)
        (progn
          ;; FIXME
          (setobj key (make-instance (switch (type :test #'string-equal)
                                       ("product" 'product)
                                       ("group" 'group)) :key key))
          (admin.post-make-fix (getobj key))
          (restas:redirect 'admin-edit-get-route :type type :key)))))

(defgeneric admin.post-make-fix (item)
  (:documentation "Perform class-specific fixes after creating instance")
  (:method (item) #| do nothing by default |#))

(defmethod admin.post-make-fix ((item product))
  (setf
   (articul item) (parse-integer (key item))
   (date-created item) (get-universal-time)
   (date-modified item) (get-universal-time))
   ;; make pointers to this product from parents
   (mapcar #'(lambda (parent)
               (push item (products parent)))
           (parents item)))

(defmethod admin.post-make-fix ((item group))
  ;; upsale
  (setf (empty item) (notany #'active (products item)))
  (mapcar #'(lambda (parent)
              (push item (groups parent)))
          (parents item))
  (when (raw-fullfilter item)
    (setf (fullfilter item) (decode-fullfilter (raw-fullfilter item)))))

(defmethod admin.post-make-fix ((item vendor))
  (let ((name (name item)))
    (when (valid-string-p name)
      (setobj (string-downcase name) item 'vendor))))

(defmethod admin.post-make-fix ((item filter))
  (mapcar #'(lambda (parent)
              (setf (gethash (key item) (filters parent)) item))
          (parents item)))

(restas:define-route admin-make-post-route ("/administration-super-panel/make" :method :post)
  (:decorators (@protected "admin"))
  (let* ((key (hunchentoot:post-parameter "key"))
         (type (anything-to-symbol (hunchentoot:post-parameter "type")))
         (item (getobj key)))
    (log:debug (hunchentoot:post-parameters*))
    (if (not (class-exist-p type))
        ;; return error
        (admin.standard-ajax-response nil "Unknown type")
        ;; else
        (progn
          (unless item ; should always be true
            (log:info "Create new item from admin panel with key: ~A" key)
            (setf item (make-instance-from-post-data type))
            (admin.post-make-fix item)
            (setobj key item)) ; adding item into storage
          ;; return success
          (admin.standard-ajax-response t)))))

(restas:define-route admin-edit-get-route ("/administration-super-panel/edit" :method :get)
  (:decorators (@protected "admin" "content"))
  ;; TODO: use type parameter
  (admin.page-wrapper
   (let* ((key (hunchentoot:get-parameter "key"))
          (item (getobj key)))
     (if item
         (soy.class_forms:formwindow
          (list :key key
                :fields (class-core.make-fields item)
                :target "edit"))
         ;; else
         "Item with specified key is not found"))))

;; TODO: rewrite using slot-specific fixers (dont forget about post-make-fix and post-unserialize)
(defgeneric admin.post-edit-fix (item)
  (:documentation "Perform class-specific fixes after changing instance slots ")
  (:method (item) #| do nothing by default |#))

(defmethod admin.post-edit-fix ((item product))
  (setf (date-modified item) (get-universal-time)))

(defmethod admin.post-edit-fix ((item group))
  (when (raw-fullfilter item)
    (setf (fullfilter item) (decode-fullfilter (raw-fullfilter item)))))

(restas:define-route admin-edit-post-route ("/administration-super-panel/edit" :method :post)
  (:decorators (@protected "admin" "content"))
  (let* ((key (hunchentoot:post-parameter "key"))
         (item (getobj key)))
    (if item
        (handler-case
            (progn
              (class-core.edit-slots item)
              (admin.post-edit-fix item)
              (slots.parents-fix item)
              (admin.standard-ajax-response t))
          (error (e) (admin.standard-ajax-response nil (format nil "ERROR: ~A" e))))
        ;; else
        (admin.standard-ajax-response nil "Item with specified key is
        not found"))))


(defun admin.pics-deleting ()
  (let* ((key (hunchentoot:post-parameter "key"))
         (p (getobj key 'product))
         (output (format nil "Product with key ~a not found" key)))
    (when p
      (rename-remove-product-pics p)
      (drop-pics-cache key)
      (setf output (format nil "Successfully deleted ~a's pics" key)))
    (soy.admin:pics-deleting (list :output output))))

(defun admin.compile-template ()
  (let ((output))
    (awhen (hunchentoot:post-parameter "name")
      (setf output
            (if (file-exists-p (pathname (merge-pathnames
                                          (pathname it)
                                          (config.get-option :paths :path-to-templates))))
                (handler-case
                    (progn
                      (servo.compile-soy it)
                      (format nil "Successfully compiled ~a" it))
                  (error (e) (format  nil "ERROR:~%~a" e)))
                (format nil "File ~a not found" it))))
    (soy.admin:compile-template (list :output output
                                      :tmpls (mapcar #'pathname-name (get-all-template-paths))))))

(defun admin.make-backup ()
  (let ((output))
    (when (hunchentoot:parameter "dobackup")
      (setf output
            (handler-case
                (progn
                  (backup.serialize-all)
                  (format nil "Successfully made backup"))
              (error (e) (format  nil "ERROR:~%~a" e)))))
    (soy.admin:make-backup (list :output output))))

(defun admin.do-action (action)
  (handler-case
      (switch ((ensure-string action) :test #'string=)
        ("do-gc"
         (sb-ext:gc :full t)
         (htmlize
          (with-output-to-string
              (*standard-output*)
            (room))))
        ("report-products"
         (create-report (format nil "products-report-~A.csv" (time.encode.backup-filename))
                        #'report.product-report)
         "DO PRODUCTS REPORT")
        ("proccess-pictures"
         (rename-convert-all)
         "DO proccess-pictures")
        ("dtd"
         (xls.update-options-from-xls)
         "DO DTD")
        ("articles-restore"
         (articles.restore)
         "RESTORE ARTICLES")
        ("main-page-restore"
         (main-page.restore)
         "RESTORE MAIN-PAGE")
        ("cartrige-restore"
         (cartrige.restore)
         "RESTORE Cartrige")
        ("static-pages-restore"
         (static-pages.restore)
         "STATIC PAGES RESTORE")
        ("groupd-restore"
         (groupd.restore)
         "GROUPD RESTORE")
        ("seo-report"
         (report.do-seo-reports)
         "SEO-REPORT")
        ("groups-products-report"
         (report.do-groups-products-report)
         "groups-products-report")
        ("keyoptions-aliases-restore"
         (report.do-alias-reports)
         action)
        ("gateway-restore"
         (gateway.load)
         "GATEWAY-RESTORE")
        (t (format nil "DON't know action ~A<br>~A" action (admin.get-info))))
    (error (e) (format  nil "ERROR:~%~a" e))))

(defun admin.parenting-content ()
  (with-hunchentoot-parameters (@products @groups)
    (when (and @products @groups)
      (mapcar #'(lambda (product)
                  (mapcar #'(lambda (group)
                              (class-core.bind-product-to-group
                               (getobj product 'product)
                               (getobj group 'group)))
                          (ensure-list @groups)))
              (remove-if #'null
                         (mapcar #'(lambda (p)
                                     (if (equal (car p) "products")
                                         (cdr p)))
                                 (hunchentoot:post-parameters*)))))
    (let* ((unparented-products (collect-storage
                                'product
                                :when-fn
                                #'(lambda (item)
                                    (and (active item)
                                         (null (parent item))
                                         (not (special-p item))))))
          (count (length unparented-products)))
      (when (< 1000 (length unparented-products))
        (setf unparented-products (subseq unparented-products 0 1000)))
      (soy.class_forms:parenting-page
       (list :products (mapcar #'(lambda (product)
                                   (soy.class_forms:unparented-product-checkbox
                                    (list :key (key product)
                                          :name (name-seo product))))
                               unparented-products)
             :length count
             :groups (slots.%view 'group-list nil "groups" nil))))))

(defun admin.vendor-seo-upload ()
  (let* ((group-key (hunchentoot:parameter "group"))
         (vendor-key (string-downcase (hunchentoot:parameter "vendor")))
         (new-text (hunchentoot:parameter "text")))
    (cond
      ((and new-text (not (and group-key vendor-key))) "Error: please specify vendor and group")
      ((and new-text (not (getobj group-key 'group))) "Error: group not found")
      ((and new-text (not (getobj vendor-key 'vendor))) "Error: vendor not found")
      (t (when new-text
           (setf (gethash group-key (seo-texts (getobj vendor-key 'vendor))) new-text))
         (soy.admin:vendor-seo-upload (list :text (awhen (getobj vendor-key 'vendor)
                                                    (gethash group-key (seo-texts it)))))))))

(defun send-mail-blacklist (key)
  (let* ((user (current-user))
         (body (format nil
                       "~A:~A"
                       (user-email user)
                       key)))
    (sendmail:send-email
     :from (config.get-option :critical :from-email)
     :to '("wolforus@gmail.com" "Errorsite@alpha-pc.com")
     :body body)))

(defun admin.black-list ()
  (let ((key (hunchentoot:parameter "key"))
        (output nil)
        (errortext nil))
    (when key
      (aif (getobj key)
           (progn
             (black-list.insert it)
             (black-list.%deactive it)
             (setf output (format nil "Товар <a href='/~A'>~A</a>:~A убран"
                                  (key it) (key it) (name-seo it)))
             (send-mail-blacklist key))
           (setf errortext "Нет такого товара")))
    (soy.admin:black-list (list :output output :errortext errortext))))

(defun limited-range (min max start num)
  (let ((range (iota num :start start)))
    (remove-if #'(lambda (n)
                   (or (<= n min)
                       (>= n max)))
               range)))

(defun admin.list-obj (&key (limit 100))
  (flet ((non-empty (string)
           (when (and string
                      (string/= string ""))
             string)))
    (let ((class (hunchentoot:parameter "class"))
          (sort-field (non-empty (hunchentoot:parameter "sortfield")))
          (sort-dir (non-empty (hunchentoot:parameter "sortdir")))
          (page (or (hunchentoot:parameter "page") "0")))
      (let* ((class-name (intern class :eshop))
             (class (find-class class-name))
             (last-page (truncate (eshop.odm:instance-count class-name) limit))
             (skip (* (parse-integer page) limit))
             (slots (append (remove-if #'(lambda (slot)
                                           (not (slot-visible (class-prototype class)
                                                              slot
                                                              :default)))
                                       (mapcar #'slot-definition-name
                                               (class-slots class)))
                            (extra-slots (class-prototype class) :default)))
             (data (eshop.odm:get-list class-name
                                       :sort (when (and sort-field sort-dir)
                                               (son (intern sort-field :eshop)
                                                    (parse-integer sort-dir)))
                                       :limit limit
                                       :skip skip)))
        (soy.admin:list-obj
         (list :currentpage (parse-integer page)
               :lastpage last-page
               :pages (limited-range 0 last-page (- (parse-integer page) 5) 11)
               :sortfield sort-field
               :sortdir (when sort-dir (parse-integer sort-dir))
               :class class-name
               :slots (mapcar #'string slots)
               :data (mapcar #'(lambda (obj)
                                 (loop
                                    :for slot :in slots
                                    :collect (render-slot obj slot :default)))
                             data)))))))

(defgeneric slot-visible (instance slot-name access)
  (:method (instance slot-name access)
    t))

(defmethod slot-visible ((instance user) (slot-name (eql 'validation-token)) access)
  nil)
(defmethod slot-visible ((instance order) (slot-name (eql 'userfamily)) access)
  nil)
(defmethod slot-visible (instance (slot-name (eql 'eshop.odm::state)) access)
  nil)
(defmethod slot-visible (instance (slot-name (eql 'eshop.odm::modified)) access)
  nil)

(defgeneric render-slot (instance slot-name access)
  (:method (instance slot-name access)
    (princ-to-string (or (slot-value instance slot-name)
                         "")))
  (:method :around (instance slot-name access)
    (if (slot-exists-p instance slot-name)
        (if (slot-boundp instance slot-name)
            (call-next-method)
            "")
        (if (member slot-name (extra-slots instance access))
            (call-next-method)
            (error "Trying to render unexisting slot ~S in object ~S" slot-name instance)))))

(defmethod render-slot ((instance order) (slot-name (eql 'items)) access)
  (length (slot-value instance slot-name)))

(defmethod render-slot ((instance user) (slot-name (eql ':order-count)) access)
  (length (eshop.odm:get-list 'order :query (son 'user instance))))

(defmethod render-slot ((instance order) (slot-name (eql 'address)) access)
  (let ((address (slot-value instance 'address)))
    (subseq address 0 (min 100 (length address)))))

(defmethod render-slot ((instance order) (slot-name (eql 'date)) access)
  (render-time '(:hour ":" :min " " :day "." :month "." :year)
               (slot-value instance 'date)))

(defgeneric extra-slots (class access)
  (:method (class access)
    nil))

(defmethod extra-slots ((class user) access)
  '(:order-count))

(defun show-admin-page (&optional (key ""))
  (soy.admin:main
   (list
    :user (current-user)
    :content
    (switch ((ensure-string key) :test #'string=)
      ("info" (soy.admin:info (list :info (admin.get-info))))
      ("objects" (admin.list-obj))
      ("actions" (soy.admin:action-buttons (list :post (hunchentoot:post-parameters*)
                                                 :info (admin.do-action
                                                        (hunchentoot:parameter "action")))))
      ("parenting" (admin.parenting-content))
      ("pics" (admin.pics-deleting))
      ("templates" (admin.compile-template))
      ("backup" (admin.make-backup))
      ("black-list" (admin.black-list))
      ("vendor-seo" (admin.vendor-seo-upload))
      ("tbmonitor" (soy.admin:table-monitoring))
      (t "Админка в разработке")))))
