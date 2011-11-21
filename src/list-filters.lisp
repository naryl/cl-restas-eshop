(in-package #:eshop)

(defclass field-filter ()
  ((fieldacc           :initarg :fieldacc         :initform nil       :accessor fieldacc)
   ;; func for type of filtering field. Unused for now (always #'list-filter.field-filter)
   (filtertype         :initarg :filtertype       :initform nil       :accessor filtertype)
   ;; func for apply to field value. For example #'> or #'string/= (lambda if necessary)
   (filterfunc         :initarg :filterfunc       :initform nil       :accessor filterfunc)
   (inclusive          :initarg :inclusive        :initform t         :accessor inclusive)
   (value              :initarg :value            :initform nil       :accessor value)))


(defmacro list-filters.inclusive-check (inclusive expression value)
  "Returns value if inclusive and expression are both t or both nil"
  `(let ((incl ,inclusive)
         (expr ,expression)
         (val ,value))
     (when (or (and incl expr)
               (not (or incl expr)))
       val)))

(defun list-filters.filter-list (items &rest filters)
  "Filters given list with given filters"
  (remove-if
   #'null
   (mapcar
    #'(lambda (item)
        (let ((res item))
          (loop for filter in filters
             do
               (setf res
                     (and res
                          (apply (filtertype filter) (list item filter)))))
          res))
    items)))


(defun list-filters.field-filter (item filter)
  (list-filters.inclusive-check
   (inclusive filter)
   (apply (filterfunc filter) (list (funcall (fieldacc filter) item)
                                    (value filter)))
   item))

(defun list-filters.limit-start (items start)
  (nthcdr start items))

(defun list-filters.limit-end (items end)
  (subseq items 0 end))

(defun list-filters.limit-region (items start length)
  (subseq items start (+ start length)))

(defun list-filters.limit-page (items page-size page-number)
  ;; numbering from 1
  (subseq items (* page-size (- page-number 1)) page-size))


(defun list-filters.get-json ()
  (print (hunchentoot:request-uri hunchentoot:*request*))
  (print (hunchentoot:post-parameters hunchentoot:*request*))
  (let* ((params (servo.alist-to-plist (hunchentoot:get-parameters hunchentoot:*request*)))
         (type (getf params :type))
         (parent (getf params :parent))
         (start (getf params :start))
         (limit (getf params :limit)))
    (setf start
          (if start
              (parse-integer start)
              0))
    (setf limit
          (if limit
              (parse-integer limit)
              50))
    (cond
      ((equalp type "groups")
       (let ((group-list (groups *global-storage*)))
         (soy.admin-table:json-data
          (list
           :number (length group-list)
           :elts (mapcar #'(lambda (g)
                             (soy.admin-table:json-group-elt
                              (list
                               :name (name g)
                               :key (key g)
                               :numprod (length (products g))
                               :order (order g)
                               :active (if (active g)
                                           "true"
                                           "false"))))
                         (list-filters.limit-region group-list start limit))))))
      ((equalp type "products")
       (if (or (null parent)
               (null (gethash parent (storage *global-storage*))))
           "Incorrect parent"
           ;;else
           ;;TODO
           "Correct parent"))
      (t "Ololo! Dont know"))))