;;;; prerender.lisp
;;; set of methods for converting templates in articles to products views,
;;; images, buttons, etc
;;; teplates look like <!--#(temp_name);arg1;arg2;...;-->

(in-package #:eshop)

;;составление строки по данным аргументам
(defun prerender-args-to-html (args)
  (let* ((type (string-trim '(#\Space) (nth 0 args))))
    (cond
      ;;вставка картинки
      ((string= type "pic")
       (let* ((size (string-trim '(#\Space) (nth 1 args)))
              (articul (string-trim '(#\Space) (nth 2 args)))
              (number (- (parse-integer
                          (string-trim '(#\Space) (nth 3 args))) 1))
              (product (getobj articul 'product))
              (picname (nth number (get-pics articul)))
              (height (nth 5 args))
              (width (nth 4 args))
              (style ""))
         (when height
           (setf style (format nil "~aheight:~apx;" style
                               (string-trim '(#\Space) height))))
         (when width
           (setf style (format nil "~awidth:~apx;" style
                               (string-trim '(#\Space) width))))
         (when (and (not picname) (get-pics articul))
           (setf picname (car (get-pics articul))))
         (when (and product (name-seo product) picname)
           (let ((path (format nil "~a/~a/~a" size articul picname)))
             (when (and (not height) (not width))
               (let ((dimensions (get-dimensions (pic-path articul picname size))))
                 (setf style (style-for-resize (getf dimensions :width)
                                               (getf dimensions :height) 600))))
             (format nil "<a href=\"/~a\" title=\"~a\">~%
                                   <img src=\"/pic/~a\" alt=\"~a\" style=\"~a\"/>~%
                                </a>~%"
                     articul (name-seo product) path (name-seo product) style)))))
      ;;вставка области для маппинга
      ((string= type "area")
       (let* ((c1 (nth 1 args))
              (c2 (nth 2 args))
              (c3 (nth 3 args))
              (c4 (nth 4 args))
              (articul (nth 5 args)))
         (format nil "<area shape=\"rect\" coords=\"~a,~a,~a,~a\"
                     href=\"#oneClickBox\" ~a>"
                 c1 c2 c3 c4
                 (soy.buttons:add-prerender-onclick
                  (list :articul articul)))))
      ;;вставка кнопки покупки
      ((string= type "buy")
       (let* ((articul (nth 1 args))
              (product (getobj articul 'product)))
         (when product
           (let ((name (name-seo product))
                 (siteprice (siteprice product))
                 (picname (car (get-pics articul))))
             (format nil "<span class=\"add\" id=\"add-img\"><big class=\"price\"><b>~a</b><var> руб.</var></big>~a"
                     (get-format-price siteprice)
                     (soy.buttons:add-product-cart
                      (list :articul articul
                            :name name
                            :siteprice siteprice
                            :pic picname)))))))
      ;;вставка счетчика
      ((string= type "tiker")
       (let* ((cnt (floor (- (time.decode.backup "2012-12-30_23:23:59")
                            (get-universal-time)) (* 60 60 24)))
              (skl (+ 1 (skls.get-count-skls cnt))))
         (format nil "<font size='6'>~A</font><br>
                         <font>~A</font><br>
                      <font size='6'>~A</font><br>"
                 (sklonenie "остался" skl) cnt (sklonenie "день" skl))))
      ;;вставка нескольких акционнных товаров
      ((string= type "rasprodaja")
       (let* ((articul (nth 1 args))
              (product (getobj articul 'product)))
         (when product
           (let ((name (name-seo product))
                 (siteprice (siteprice product))
                 (picname (car (get-pics articul))))
             (format nil "<b><a href=\"/~a\">~a</a></b><br><br>
<span class=\"add\" id=\"add-img\">
                           ~a
                           <big class=\"price red\"><b>~a</b></big><var class=\"red\"> руб.</var><br>
~a"
                     articul
                     (name-seo product)
                     (if (> (price product) siteprice)
                         (format nil "<big class=\"strike price\">~a</big><var> руб.</var><br>" (get-format-price (price product)))
                         "")
                     (get-format-price siteprice)
                     (soy.buttons:add-product-rasp
                      (list :articul articul
                            :name name
                            :siteprice siteprice
                            :pic picname)))))))
            ;;вставка нескольких акционнных товаров
      ((string= type "sandwich")
       (format nil "~{~a~}"
                 (loop :for articul :in (cdr args)
                    :when (and articul (getobj articul 'product))
                    :collect (let ((product (getobj articul 'product)))
                               (soy.buttons:add-product-onclick-func
                                (list  :articul articul
                                       :name (name-seo product)
                                       :siteprice (siteprice product)
                                       :pic (car (get-pics articul))))))))
      ((string= type "marth8")
       (let* ((articul (nth 1 args))
              (product (getobj articul 'product)))
         (when product
           (soy.product:marth8
            (list :pic (car (get-pics articul))
                  :articul articul
                  :name (name-seo product)
                  :keyoptions (subseq (render.get-keyoptions product) 0 4)
                  :price (get-format-price (siteprice product)))))))
      ((string= type "patric")
       (let* ((type (nth 1 args))
              (articul (nth 2 args))
              (product (getobj articul 'product)))
         (when product
           (string-case type
             ("green" (soy.product:green_item (list :item (prerender.product-view (articul product)))))
             ("yellow" (soy.product:yellow_item (list :item (prerender.product-view (articul product)))))))))
      ((string= type "freedelivery")
       (let* ((products '(200206 198039 202561 194840 174088 172230 202495 188653 172222 174086 185908
                          194785 164746 194842 172604 164743 190959 164741 182701 169431 172859 198050
                          202528 165040 171003 197761 192971 164939 181880 165058 181402 165038 169343
                          167665 188683 181403 197762 205304 188694 192298 181404 186404 190919 192892
                          170321 190920 198045 197729 186374 166684 171547 188640 190938 167210 165027
                          165768 174093 183861 174092 164588 164591 165033 165788 164416 188692 165037
                          198040 167362 192297 167757 181885 164964 167321 197730 164823 172884 198123
                          192903)))
         (soy.product:free_delivery_items
          (list :items
                (mapcar #'prerender.product-view
                           products)))))
      ((string= type "price")
       (let* ((articul (nth 1 args))
              (product (getobj articul 'product)))
         (when product
           (let ((siteprice (siteprice product)))
             (format nil "~a"
                     (get-format-price siteprice))))))
      (t
       (format nil "<!-- unknown format -->~%")))))

(defun prerender.product-view (key)
  (let ((product (getobj (write-to-string key))))
  (when product
    (list :pic (car (get-pics (key product)))
          :articul (articul product)
          :name (name-seo product)
          :keyoptions (subseq (render.get-keyoptions product) 0 4)
          :price (get-format-price (siteprice product))))))


(defun prerender-string-replace (string)
  (let* ((start (search "<!--#" string)) (end (search ";-->" string)))
    (if (null start)
        string
        (concatenate 'string (subseq string 0 start)
                     (prerender-args-to-html
                      (split-sequence #\;
                                      (subseq string (+ 5 start) (+ 1 end))
                                      :remove-empty-subseqs t))
                     (prerender-string-replace (subseq string (+ 4 end)))))))
