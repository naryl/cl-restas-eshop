;;;; cart.lisp

(in-package #:eshop)

(defun get-order-id ()
  "Thread-unsafe. Somebody else may use this id in which case
  mongo:insert throws an error and you need to get another id."
  (1+ (order-id
       (first
        (eshop.odm:get-list 'order
                            :sort (son 'eshop.odm::key -1)
                            :limit 1)))))

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

(defun make-order-obj (phone email city address username userfamily ekk bonuscount
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
    (loop :until order
       :for order-id := (get-order-id)
       :for order := (handler-case
                         (make-instance 'order
                                        :key order-id
                                        :user (current-user)
                                        :phone phone
                                        :email email
                                        :address address
                                        :username username
                                        :userfamily userfamily
                                        :bonuscard ekk
                                        :bonuscount bonuscount
                                        :comment comment
                                        :delivery delivery
                                        :items products
                                        :state 0)
                       (eshop.odm:duplicate-key-error () nil))
       :finally (return order))))
