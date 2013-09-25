
(in-package eshop)

(defun load-sexp (filename)
  (with-open-file (in filename :direction :input)
    (loop
      (let ((sexp (read in nil :eof)))
        (when (eq sexp :eof)
          (return))
        (setf (getf sexp :items)
              (mapcar (lambda (o)
                        (apply 'make-instance 'order-item o))
                      (getf sexp :items)))
        (let ((order (apply 'make-instance 'order sexp)))
          (fix-order order sexp)
          (values))))))

(defun fix-order (order sexp)
  (let ((calculated (order-total order))
        (reported (getf sexp :total)))
    (with-slots (items delivery) order
      (unless (eql calculated reported)
        (let* ((total (- reported delivery))
               (counts (solve-counts (mapcar #'order-item-price items) total)))
          (loop
             :for item :in items
             :for count :in counts
             :do (setf (order-item-count item) count))
          (eshop.odm:setobj order'items items))))))

(defun solve-counts (prices sum)
  (when (= 1 (length prices))
    (let ((count (/ sum (first prices))))
      (return-from solve-counts
        (if (integerp count)
            (list count)
            nil))))
  (loop
     :for count from 0
     :until (> (* count (first prices)) sum)
     :do (let ((rest (solve-counts (rest prices)
                                   (- sum (* (first prices) count)))))
           (when rest
               (return-from solve-counts (list* count rest)))))
  (cerror "Ignore object" "Can't solve linear equation"))
