;;;; oneclickcart.lisp

(in-package #:eshop)

(defun oneclick-sendmail1 (phone articul name email)
  (let ((client-mail)
        (mail-file)
        (filename)
        (tks-mail)
        (order-id (get-order-id))
        (count)
        (pricesum)
        (products)
        (cart (list (list (cons :id articul) (cons :count 1)))))
    (multiple-value-bind (lst cnt sm) (newcart-cart-products cart)
      (setf products (remove-if #'null lst))
      (setf count cnt)
      (setf pricesum sm))
    (when (zerop pricesum)
      (setf pricesum ""))
    (let ((real-comment (format nil "Заказ через форму один клик ~@[!!! Предзаказ !!!~]"
                                (preorder (getobj articul 'product)))))
      (make-order-obj order-id phone email "" "" name "" ""
                      0 real-comment 0 products))
    (setf client-mail
          (soy.sendmail:clientmail
           (list :datetime (time.get-date-time)
                 :order_id order-id
                 :name (report.convert-name name)
                 :family "" ;; Фамилия не передается отдельно
                 :paytype "Наличными"
                 :deliverytype "Самовывоз"
                 :addr "Левашовский пр., д.12"
                 :bankaccount ""
                 :phone phone
                 :email email
                 :comment (format nil "Заказ через форму один клик ~@[!!! Предзаказ !!!~]"
                                  (preorder (getobj articul 'product)))
                 :products products
                 :deliverysum 0
                 :itogo pricesum)))
    (setf mail-file
          (list :order_id order-id
                :ekk ""
                :name (report.convert-name name)
                :family ""
                :addr "Левашовский пр., д.12"
                :phone phone
                :email email
                :isdelivery "Самовывоз"
                :date (time.get-date)
                :time (time.get-time)
                :comment (format nil "Заказ через форму один клик ~@[!!! Предзаказ !!!~]"
                                 (preorder (getobj articul 'product)))
                :products products))
    (setf filename (format nil "~a_~a.txt" (time.get-short-date) order-id))
    ;;сорханение заказа
    (save-order-text order-id client-mail)
    ;; удаление страных символов
    (setf client-mail (remove-if #'(lambda(c) (< 10000 (char-code c))) client-mail))
    (setf tks-mail (remove-if #'(lambda(c) (< 10000 (char-code c))) (soy.sendmail:mailfile mail-file)))
    (email.send-order-details order-id (soy.sendmail:tks-clientmail-wrapper
                                        (list :body client-mail
                                              :ip (tbnl:real-remote-addr)
                                              :agent (tbnl:user-agent)))
                              filename tks-mail)
    (when (email.valid-email-p email)
      (email.send-client-mail email order-id client-mail))
    order-id))


(defclass oneclickcartanswer ()
  ((phone     :initarg :phone     :initform nil   :accessor phone)
   (orderid   :initarg :orderid   :initform nil   :accessor orderid)
   (errorid   :initarg :errorid   :initform nil   :accessor errorid)
   (articul   :initarg :articul   :initform nil   :accessor articul)
   (total     :initarg :total     :initform nil   :accessor total)
   (name      :initarg :name      :initform nil   :accessor name)
   (group     :initarg :group     :initform nil   :accessor group)
   (price     :initarg :price     :initform nil   :accessor price)
   ))

(defun oneclickcart.make-common-order (request-get-plist)
  (let* ((telef (getf request-get-plist :phone))
         (name (getf request-get-plist :name))
         (articul (getf request-get-plist :article))
         (email (getf request-get-plist :email))
         (pr (getobj articul 'product))
         (order-id)
         (error-id 0)
         (answer (make-instance 'oneclickcartanswer :phone telef)))
    (if telef
        (if articul
            (if pr
                (setf order-id (oneclick-sendmail1 telef articul name email))
                (setf error-id 3)) ;; no such product
            (setf error-id 1)) ;; no articul in parameters
        (setf error-id 2)) ;; no phone number in parameters
    (setf (orderid answer) order-id)
    (setf (errorid answer) error-id)
    (awhen (getobj articul 'product)
      (setf (articul answer) articul
            (total answer) (siteprice it)
            (name answer) (name-seo it)
            (group answer) (aif (car (parents it))
                                (name it)
                                "")
            (price answer) (siteprice it)))
    (st-json:write-json-to-string
     (son "articul" (slot-value answer 'articul)
          "errorid" (slot-value answer 'errorid)
          "group" (slot-value answer 'group)
          "name" (slot-value answer 'name)
          "orderid" (slot-value answer 'orderid)
          "phone" (slot-value answer 'phone)
          "price" (slot-value answer 'price)
          "total" (slot-value answer 'total)))))
