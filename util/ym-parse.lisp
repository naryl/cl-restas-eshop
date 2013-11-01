
(defpackage eshop.parse-ym
  (:use :cl
        :eshop.utils
        :alexandria
        :cl-html-parse
        :cl-html-query
        :drakma))

(in-package eshop.parse-ym)

(defvar *data* (make-hash-table))

(defun get-img-data ()
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
          (write-byte-vector-into-file (drakma:http-request img-url)
                                       path)))
      (sleep 1))
    (eshop::rename-convert-all :from path)))

(defun get-product-page (product)
  (let+ (((:values body _ _ uri)
          (drakma:http-request
           (format nil "http://market.yandex.ru/search.xml?text=~A&cvredirect=2"
                   (hunchentoot:url-encode product))
           :preserve-uri t))
         (have-modelid (second (multiple-value-list
                                (cl-ppcre:scan-to-strings "modelid=(\\d+)"
                                                          (puri:uri-query uri))))))
    (list
     (parse-html body)
     (when have-modelid (parse-integer (aref have-modelid 0))))))

(defun get-product-image-url (page product)
  (when (equal product
               (tag-attr (query page ".b-model-pictures__big a meta") :content))
    (let ((img-span (query page ".b-model-pictures__big")))
      (or (tag-attr (query img-span "a") :href)
          (tag-attr (query img-span "img") :src)))))
