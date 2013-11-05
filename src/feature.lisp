(in-package #:eshop)

(alexandria:define-constant +phone-next-template+
    (make-instance 'sendmail:email
                   :from (config.get-option :critical :from-email)
                   :type "text" :subtype "html")
  :test (constantly t)
  :documentation "Template for email to client about his/her order")

(defun email.phone-next-mail (body)
  (declare (string body))
  (sendmail:send-email-with-template
   +phone-next-template+
   :to (config.get-option :critical :order-emails)
   :subject (format nil "www.320-8080.ru - ЗАКАЗ ЗВОНКА")
   :body body))

(defclass phone-next.answer ()
  ((phone     :initarg :phone     :initform nil   :accessor phone)
   (errorid   :initarg :errorid   :initform nil   :accessor errorid)))

(restas:define-route phone-next-route ("/phone_next")
  (:decorators '@timer)
  (phone-next (request-get-plist)))

(defun phone-next (request-get-plist)
  (let* ((phone (getf request-get-plist :phone))
         (error-id 0)
         (answer (make-instance 'phone-next.answer :phone phone)))
    (if phone
        (email.phone-next-mail (soy.sendmail:phonemail (list :phone phone)))
        (setf error-id 2)) ;; no phone number in parameters
    (setf (errorid answer) error-id)
    (write-object-to-json answer)))


(defvar *sbr-orders* (make-hash-table :test #'equal))


(defparameter *sbr-result* nil)

(restas:define-route sberbank-route ("/sberbank_test")
  (:decorators '@timer)
  (incf *sbr-orders-id*)
  (setf *sbr-result* (drakma:http-request (format nil "https://3dsec.sberbank.ru/payment/rest/registerPreAuth.do?userName=320_8080-api&password=320_8080&orderNumber=~A&amount=54321&returnUrl=http%3A%2F%2F320-8080.ru%2Fresult" *sbr-orders-id*))))


(restas:define-route checkout1-route ("/checkout1")
  (:decorators '@timer)
  (newcart-show1))

(restas:define-route checkout1-post-route ("/checkout1" :method :post)
  (:decorators '@timer)
  (newcart-show1))

(restas:define-route thanks1-route ("/thanks1")
  (:decorators '@timer)
  (thanks-page1))

;;отображение страницы
(defun newcart-show1 (&optional (request-str ""))
  (declare (ignore request-str))
  (when (assoc "operation_id" (hunchentoot:post-parameters hunchentoot:*request*)  :test #'string=)
    (newcart-yandex-cookie))
  (let ((cart-cookie (hunchentoot:url-decode (hunchentoot:cookie-in "cart")))
        (cart)
        (products)
        (count 0)
        (pricesum 0)
        (bonuscount nil))
    (when (not (null cart-cookie))
      (setf cart (st-json:read-json-from-string cart-cookie))
      (multiple-value-bind (lst cnt sm bc) (newcart-cart-products cart)
        (setf products (remove-if #'null lst))
        (setf count cnt)
        (setf pricesum sm)
        (if (and bc
                 (not (equal bc 0)))
            (setf bonuscount bc))))
    (if (and (not (null products))
             (< 0 pricesum)
             (< 0 count))
        (progn
          (soy.newcart:fullpage1 (list :head (soy.newcart:head)
                                      :leftcells (soy.newcart:leftcells
                                                  (list :bonuscount bonuscount
                                                        :bonusname (if bonuscount
                                                                       (nth (skls.get-count-skls bonuscount)
                                                                            (list "бонус" "бонуса" "бонусов")))))
                                      :rightcells (soy.newcart:rightcells
                                                   (list :notfinished "true"
                                                         :deliverysum pricesum
                                                         :productscount count
                                                         :tovar (newcart-tovar count)
                                                         :products (mapcar #'soy.newcart:product-item products))))))
        (progn
          ;; страница для пустой корзины с автоматическим редиректом на главную
          (soy.newcart:fullpage (list :head (soy.newcart:head-redirect (list :timeout 5
                                                                             :url "/"))
                                      :leftcells (soy.newcart:leftcells-empty)))))))


(defvar *sbr-order-id* 100000)

(defun get-sbr-order-id ()
  "Generate pseudo-unique order number"
  (let ((current-order-id *sbr-order-id*)
        (order-id-pathname #P"/home/webadmin/Dropbox/htconf/sbr-order-id.txt"))
    (if *sbr-order-id*
        (progn
          (incf *sbr-order-id*)
          (with-open-file (file order-id-pathname
                                :direction :output
                                :if-exists :supersede
                                :external-format :utf-8)
            (format file "~a" *sbr-order-id*))
          current-order-id)
        ;;else
        (progn
          ;;если в файле шлак, то сбрасываем счетчик заказов до 1
          (setf *sbr-order-id*
                (handler-case
                    (parse-integer
                     (alexandria:read-file-into-string
                      order-id-pathname))
                  (SB-INT:SIMPLE-PARSE-ERROR () 1)
                  (SB-INT:SIMPLE-FILE-ERROR () 1)))
          (get-sbr-order-id)))))


;; страница информации об отправленном заказе
(defun thanks-page1 ()
  (let ((cart) ;; товары
        (user) ;; данные о пользователе
        (products)
        (count)  ;; количество товаров в корзине
        (pricesum) ;; сумма заказа
        (bonuscount)) ;; сумма бонусов
    ;; кукисы пользователя
    (mapcar #'(lambda (cookie)
                (switch ((car cookie) :test #'string=)
                  ("cart" (setf cart (st-json:read-json-from-string
                                      (hunchentoot:url-decode(cdr cookie)))))
                  ("user-nc" (setf user (st-json:read-json-from-string
                                         (hunchentoot:url-decode(cdr cookie)))))
                  (t nil)))
            (hunchentoot:cookies-in hunchentoot:*request*))
    ;;если кукисы не пустые
    (when (and (not (null cart))
               (not (null user)))
      (multiple-value-bind (lst cnt sm bc) (newcart-cart-products cart)
        (setf products (remove-if #'null lst))
        (setf count cnt)
        (setf pricesum sm)
        (if (and bc
                 (not (equal bc 0)))
            (setf bonuscount bc))))
    (if (and (not (null products))
             (not (null (newcart-get-data-from-alist :phone user))))
        ;; если в заказе есть валидные товары и телефон пользователя
        ;; генерация идентификатора заказа происходит только если заказ валиден
        (let ((order-id (get-sbr-order-id)) ;; генерируем ID заказа
              (deliverysum 0) ;;цена доставки
              (client-mail) ;; текст письма с информацие о заказе для клиента
              (mail-file) ;; информация для ТКС
              (tks-mail) ;; файл с информацией о заказе для ТКС
              (filename)) ;;
          (multiple-value-bind (phone delivery-type name email city addr courier_comment pickup pickup_comment payment bankaccount ekk family)
              (newcart-user user)
            ;; Временно доставка 300 на все
            ;; существует два вида доставки: курьером и самовывоз (express | pickup)
            (if  (string= delivery-type "express")
                 (setf deliverysum (yml.get-delivery-price (newcart-cart-products cart))))
            (if  (and (string= delivery-type "pickup")
                      (string= pickup "pickup-3"))
                 (setf deliverysum 100))
            (let ((result-obj)
                  (url (format nil "https://3dsec.sberbank.ru/payment/rest/registerPreAuth.do?userName=320_8080-api&password=320_8080&orderNumber=~A&amount=~A&returnUrl=http%3A%2F%2F320-8080.ru%2Fr%2F~A" order-id (* (+ pricesum deliverysum) 100) order-id)))
              (setf result-obj (st-json:read-json-from-string (drakma:http-request url)))
              (setf (gethash order-id *sbr-orders*) result-obj)
              (hunchentoot:redirect (cdr (assoc :form-url result-obj)) :code 301)
              ))))))

;; (define-tracing-route sberbank-route ("/sberbank_test")
;;   (incf *sbr-orders-id*)
;;   (drakma:http-request "https://3dsec.sberbank.ru/payment/rest/registerPreAuth.do"
;;                          :method :get
;;                          :parameters (list
;;                                       (cons "userName" "320_8080-api")
;;                                       (cons "password" "320_8080")
;;                                       (cons "orderNumber" *sbr-orders-id*)
;;                                       (cons "amount" "54321")
;;                                       (cons "returnUrl" "http%3A%2F%2F320-8080.ru%2Fresult"))))

;; (defmethod restas:render-object ((designer eshop-render) (object product))
;;   (aif (get-option object "Secret" "Дубль")
;;        (hunchentoot:redirect (concatenate 'string "/" it) :code 301))



(defvar t.*tmpl*)


(let ((cnt))
  (process-storage #'(lambda (p) (print p))
                   'product #'(lambda (p) (search "<h1>" (seo-text p))))
  cnt)
