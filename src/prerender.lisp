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
       (let* ((products '(
                          208272 208273 208274 208277 208275 208276 208290 208332 208333 208291 208292
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
                          155996 163986 169263 169440 209026 182721 214664 207500 207504 207503 207502 216730 207501)))
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
  (when (and product
             (> (length (render.get-keyoptions product)) 3))
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
