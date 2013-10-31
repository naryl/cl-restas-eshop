
(defpackage eshop.parse-ym
  (:use :cl
        :cl-html-parse
        :alexandria
        :drakma))

(in-package eshop.parse-ym)

(defvar *data* (make-hash-table))

(defun get-img-data ()
  (setf *data* (make-hash-table))
  (let ((group (eshop::getobj "watch"))
        (path (merge-pathnames "market-images/"
                               (eshop::config.get-option :paths :path-to-logs))))
    (dolist (product (slot-value group 'eshop::products))
      (let ((articul (slot-value product 'eshop::articul))
            (name (slot-value product 'eshop::name-seo)))
        (destructuring-bind (page productid)
            (get-product-page name)
          (let ((path (merge-pathnames (format nil "~A/~A.jpg" articul name)
                                       path))
                (img-url (get-product-image-url page)))
            (setf (gethash articul *data*)
                  (list name productid img-url))
            (when img-url
              (ensure-directories-exist path)
              (write-byte-vector-into-file (drakma:http-request img-url)
                                           path)))))
      (sleep 1))
    (eshop::rename-convert-all :from path)))

(defun get-product-page (product)
  (multiple-value-bind (body code headers uri)
      (drakma:http-request
       (format nil "http://market.yandex.ru/search.xml?text=~A&cvredirect=2"
               (hunchentoot:url-encode product))
       :preserve-uri t)
    (declare (ignore code headers))
    (let ((have-modelid (second (multiple-value-list
                                 (cl-ppcre:scan-to-strings "modelid=(\\d+)"
                                                           (puri:uri-query
                                                            uri))))))
      (list
       (parse-html body)
       (when have-modelid (parse-integer (aref have-modelid 0)))))))

(defun get-product-image-url (page)
  (let ((img-span (query page ".b-model-pictures__big")))
    (or (tag-attr (query img-span "a") :href)
        (tag-attr (query img-span "img") :src))))

;;;; JQUERY

(defun tag-attrs (tag)
  (rest (ensure-list (first tag))))

(defun tag-attr (tag attr)
  (getf (tag-attrs tag) attr))

(defun query (src-html query)
  (let ((tokens (split-sequence:split-sequence #\Space query)))
    (loop :for token :in (list* nil tokens)
       :for html = src-html :then (query-one html (parse-selector token))
       :finally (return html))))

(defun query-one (html selector)
  (walk html #'(lambda (tag)
                 (apply-selector tag selector))))

(defun parse-selector (selector)
  (multiple-value-bind (match tokens)
      (cl-ppcre:scan-to-strings "^([a-zA-Z-_]*)(([#.])([a-zA-Z-_]*))?$" selector)
    (declare (ignore match))
    (let ((tag (elt tokens 0))
          (type (elt tokens 2))
          (value (elt tokens 3)))
      (list :tag (when (string/= "" tag)
                   (make-keyword (string-upcase tag)))
            :type (when type
                    (ecase (elt type 0)
                      (#\# :id)
                      (#\. :class)))
            :value value))))

(defun apply-selector (element selector)
  (labels ((select (&key tag type value)
             (when (or (not tag)
                       (eq tag (first element)))
               (when (or (and (not type)
                              (not value))
                         (equal value (getf (cdr element) type)))
                 t))))
    (apply #'select selector)))

(defun walk (html matcher)
  (labels ((walk (html)
             (if (funcall matcher (ensure-list (first html)))
                 html
                 (some #'walk (remove-if-not #'listp (rest html))))))
    (if (eq (caar html) :!doctype)
        (walk (second html))
        (walk html))))

(defun xpath (html &rest route)
  (let ((child (assoc (first route) (rest html)
                      :key #'(lambda (item) (if (listp item) (first item) item)))))
    (if (rest route)
        (apply #'xpath child (rest route))
        child)))
