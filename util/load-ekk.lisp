
(defun load-ekk (filename)
  (eshop::dolines (line filename)
    (apply #'make-instance 'eshop::bonuscard (read-from-string line))))
