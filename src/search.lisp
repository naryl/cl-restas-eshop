;;;; search.lisp

(in-package #:eshop)

(defun search-engine (q size)
  (let* ((wordlist (mapcar #'string-downcase (split-sequence #\Space q)))
         (results (make-hash-table :test #'equal))
         (result-list nil)
         (sorted-list nil))
    (process-storage
     #'(lambda (val)
         (let ((key (key val)))
           (when (and (active val)
                      (parent val))
             (let ((name (string-downcase (format nil "~a" (name-seo val)))))
               (mapcar #'(lambda (word)
                           (let* ((search-result (search word name))
                                  (tmp (gethash key results (list :name name :rel 0))))
                             (when search-result
                               (setf (getf tmp :rel) (1+ (getf tmp :rel)))
                               (setf (gethash key results) tmp))))
                       wordlist)))))
     'product)
    ;; Преобразуем хэш с элементами вида (list :name "xxx" :rel "yyy") в список для сортировки
    (maphash #'(lambda (key val)
                 (push (list* :id key val) result-list))
             results)
    ;; Сортируем список
    (setf sorted-list (sort result-list #'(lambda (a b) (> (getf a :rel) (getf b :rel)))))
    ;; Обрезаем результаты
    (when (< size (length sorted-list))
      (setf sorted-list (subseq sorted-list 0 size)))
    ;; Возвращаем список продуктов
    (mapcar #'(lambda (x)
                (getobj (getf x :id) 'product))
            sorted-list)))


(defun get-match-products (q)
  (when (string/= q "")
    (let ((articul (parse-integer q :junk-allowed t)))
      (if (null articul)
          (search-engine q 50)
          ;; else
          (let ((result (getobj (format nil "~a" articul) 'product)))
            (if (null result)
                (search-engine q 50)
                (list result)))))))

(defun get-safe-url-decode-value (param)
  param)


(defun search-page ()
  (let* ((q (hunchentoot:get-parameter "q"))
         (search-string (strip q))
         (url-decoded (get-safe-url-decode-value search-string)))
    (cond ((string= q "")              (make-output "Введите поисковый запрос!"))
          ((null url-decoded)          (make-output))
          ((> 3 (length url-decoded))  (make-output "Слишком короткий поисковый запрос"))
          (t (let ((search-result (get-match-products (strip url-decoded))))
               (if (null search-result)
                   (make-output)
                   (make-output (prefer search-result))))))))

(defun prefer (products)
  (soy.catalog:centerproduct
   (list
    ;; :producers (group:make-vendor-filter (parent object))
    :accessories (soy.catalog:accessories)
    :products (loop
                 :for product
                 :in (remove-if-not #'active products)
                 :collect (render.view product)))))



(defun make-output (&optional (centercontent nil))
  (default-page
      (soy.catalog:content
       (list :name "Поиск мысли..."
             :breadcrumbs "<a href=\"/catalog\">Каталог</a> / Поиск"
             :menu (render.menu)
             :rightblocks (list (soy.catalog:rightblock1)
                                (soy.catalog:rightblock2))
             :subcontent (if (null centercontent)
                             "Ничего не найдено"
                             (format nil "~a" centercontent))))))


;;; prefix-search by name of product
(defparameter *labels-tips* (search-tips:build-search-tips
                             (map 'vector #'(lambda (product)
                                              (make-instance 'search-tips:search-tip
                                                             :tip (name-seo product)
                                                             :weight (price product)
                                                             :info (key product)))
                                  (collect-storage 'product))))

;; request: (mapcar #'search-tips:info (search-tips:max-k-tips-by-prefix *labels-tips* "acer" 10))
