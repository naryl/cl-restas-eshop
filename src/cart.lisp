;;;; cart.lisp

(in-package #:eshop)

(defvar *order-id* nil)

(defun get-order-id ()
  "Generate pseudo-unique order number"
  (let ((current-order-id *order-id*)
        (order-id-pathname (config.get-option :critical :path-to-order-id-file)))
    (if *order-id*
        (progn
          (incf *order-id*)
          (with-open-file (file order-id-pathname
                                :direction :output
                                :if-exists :supersede
                                :external-format :utf-8)
            (format file "~a" *order-id*))
          current-order-id)
        ;;else
        (progn
          ;;если в файле шлак, то сбрасываем счетчик заказов до 1
          (setf *order-id*
                (handler-case
                    (parse-integer
                     (alexandria:read-file-into-string
                      order-id-pathname))
                  (SB-INT:SIMPLE-PARSE-ERROR () 1)
                  (SB-INT:SIMPLE-FILE-ERROR () 1)))
          (get-order-id)))))

;;проверка заказа на валидность
;;TODO сделать полную проверку
(defun order-valid-p (products)
  (not (null products)))

(defun save-order-text (file-name body)
  (when (config.get-option :start-options :release)
    (let ((filename (merge-pathnames (format nil "orders/~A.html" file-name)
                                     (config.get-option :paths :path-to-dropbox))))
      (with-open-file
          (stream filename :direction :output :if-exists :supersede)
        (format stream "~a" body)))))

(defun make-order-obj (order-id phone email city address username userfamily ekk bonuscount
                       comment delivery products)
  (let ((address (make-instance 'address :city city :address address))
        (products (mapcar #'(lambda (product)
                              (flet ((@ (field)
                                         (getf product field)))
                                (make-instance 'order-item
                                               :article (@ :articul)
                                               :count (@ :count)
                                               :name (@ :name)
                                               :price (@ :price)
                                               :site-price (@ :siteprice)
                                               :bonuscount (@ :bonuscount))))
                          products)))
    (make-instance 'order
                   :key order-id
                   :user (current-user)
                   :phone phone
                   :email email
                   :address address
                   :username username
                   :userfamily userfamily
                   :ekk ekk
                   :bonuscount bonuscount
                   :comment comment
                   :delivery delivery
                   :items products
                   :state 3)))
