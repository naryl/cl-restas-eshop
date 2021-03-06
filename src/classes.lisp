;;;; classes.lisp

(in-package #:eshop)

(class-core.make-class-and-methods
 product
 ((:name key               :initform ""                     :disabled t     :type string      :serialize t)
  (:name articul           :initform nil                    :disabled t     :type int         :serialize t)
  (:name name-provider     :initform ""                     :disabled nil   :type string      :serialize t)
  (:name name-seo          :initform ""                     :disabled nil   :type string      :serialize t)
  (:name siteprice         :initform 0                      :disabled nil   :type int         :serialize t)
  (:name erp-price         :initform 0                      :disabled t     :type int         :serialize t)
  (:name erp-class         :initform ""                     :disabled t     :type string      :serialize t)
  (:name delta-price       :initform 0                      :disabled nil   :type int         :serialize t)
  (:name bonuscount        :initform 0                      :disabled nil   :type int         :serialize t)
  ;; (:name delivery-price    :initform nil                    :disabled nil   :type int         :serialize t)
  (:name active            :initform t                      :disabled nil   :type bool        :serialize nil)
  (:name preorder          :initform nil                    :disabled nil   :type bool        :serialize t)
  (:name newbie            :initform t                      :disabled nil   :type bool        :serialize t)
  (:name sale              :initform t                      :disabled nil   :type bool        :serialize t)
  (:name parents           :initform nil                    :disabled nil   :type group-list  :serialize t)
  (:name date-modified     :initform (get-universal-time)   :disabled t     :type time        :serialize t)
  (:name date-created      :initform (get-universal-time)   :disabled t     :type time        :serialize t)
  (:name seo-text          :initform ""                     :disabled nil   :type textedit    :serialize t)
  (:name count-transit     :initform 0                      :disabled t     :type int         :serialize t)
  (:name count-total       :initform 0                      :disabled t     :type int         :serialize t)
  (:name optgroups         :initform nil                    :disabled t     :type optgroups   :serialize nil)
  (:name vendor            :initform ""                     :disabled nil   :type string      :serialize t))
 :storage-size 70000)


(class-core.make-class-and-methods
 group
 ((:name key                 :initform nil                             :disabled t     :type string                    :serialize t)
  (:name yml-id              :initform (yml.get-next-yml-id)           :disabled t     :type int                       :serialize t)
  (:name parents             :initform nil                             :disabled nil   :type group-list                :serialize t)
  (:name name                :initform nil                             :disabled nil   :type string                    :serialize t)
  (:name active              :initform nil                             :disabled nil   :type bool                      :serialize t)
  (:name empty               :initform nil                             :disabled t     :type bool                      :serialize nil)
  (:name order               :initform 1000                            :disabled nil   :type int                       :serialize t)
  (:name ymlshow             :initform nil                             :disabled nil   :type bool                      :serialize t)
  (:name pic                 :initform nil                             :disabled nil   :type string                    :serialize t)
  (:name icon                :initform nil                             :disabled nil   :type string                    :serialize t)
  ;; (:name delivery-price      :initform nil                             :disabled nil   :type int?                      :serialize t)
  (:name groups              :initform nil                             :disabled t     :type group-list                :serialize nil)
  (:name products            :initform nil                             :disabled t     :type product-list              :serialize nil)
  (:name filters             :initform (make-hash-table :test #'equal) :disabled t     :type undefined                 :serialize nil)
  (:name fullfilter          :initform nil                             :disabled t     :type undefined                 :serialize nil)
  (:name raw-fullfilter      :initform nil                             :disabled nil   :type textedit-raw              :serialize t)
  (:name vendors             :initform (make-hash-table :test #'equal) :disabled t     :type undefined #||hash-table||#:serialize nil)
  (:name seo-text            :initform nil                             :disabled nil   :type textedit                  :serialize t)
  (:name upsale-links        :initform nil                             :disabled nil   :type group-list                :serialize t)
  (:name keyoptions          :initform nil                             :disabled nil   :type keyoptions                :serialize t)
  (:name catalog-keyoptions  :initform nil                             :disabled nil   :type catalog-keyoptions        :serialize t)
  (:name life-time           :initform 100                             :disabled nil   :type int                       :serialize t))
 :storage-size 400)


(class-core.make-class-and-methods
 vendor
 ((:name key       :initform ""                              :disabled t   :type string             :serialize t)
  (:name name      :initform ""                              :disabled nil :type string             :serialize t)
  (:name alias     :initform ""                              :disabled nil :type string             :serialize t)
  (:name seo-texts :initform (make-hash-table :test #'equal) :disabled t   :type textedit-hashtable :serialize t))
 :storage-size 300)

(defclass group-filter ()
  ((name              :initarg :name            :initform nil       :accessor name)
   (base              :initarg :base            :initform nil       :accessor base)
   (advanced          :initarg :advanced        :initform nil       :accessor advanced)))


(defgeneric serialize-p (object)
  (:documentation "Checks whether object needs serialization (used in backup methods)")
  (:method (object) t)
  (:method ((object filter))
           (serialize object)))

;; TODO: rewrite options store mechanism (with using string literal -> id convertion)
(defmacro with-option1 (product optgroup-name option-name body)
  `(mapcar #'(lambda (optgroup)
               (when (string= (getf optgroup :name) ,optgroup-name)
                 (let ((options (getf optgroup :options)))
                   (mapcar #'(lambda (option)
                               (if (string= (getf option :name) ,option-name)
                                   ,body))
                           options))))
           (optgroups ,product)))

;; Beware of names! (dumb-written macro with-option1)
(defun get-option (product opgroup optname)
  (declare (product product) (string opgroup optname))
  (let (res)
    (with-option1 product
      opgroup optname
      (setf res (getf option :value)))
    res))

;; TODO: handle case when no option found
(defun set-option (product opgroup optname value)
  (declare (product product) (string opgroup optname))
  (with-option1 product
    opgroup optname
    (setf (getf option :value) value)))

(defmethod price ((product product))
  (+ (siteprice product) (delta-price product)))

(defun classes.has-vendor-seo-text (group vendor-key)
  "Chech whether there is vendor's seo-text for given group"
  (and group (valid-string-p vendor-key)
       (let ((vendor-obj (getobj (string-downcase vendor-key) 'vendor)))
         (and vendor-obj (gethash (key group) (seo-texts vendor-obj))))
       t))

(defun classes.get-group-seo-text (group &optional vendor-key)
  "Get group seotext or vendor seotext."
  (declare (group group))
  (let ((vendor-object (when (valid-string-p vendor-key)
                         (getobj (string-downcase vendor-key) 'vendor))))
    (if vendor-object
        (aif (gethash (key group) (seo-texts vendor-object))
             it
             "")
        ;;else
        (seo-text group))))


(defgeneric parent (item)
  (:documentation "Returns main parent of item"))

(defmethod parent (item)
  "Standard parent, when no more specific method defined"
  (first (parents item)))

(defmethod name ((item filter))
  (getf (data item) :name))

(defun get-root-parent (item)
  (when item
    (let ((parent (parent item)))
      (if (null parent)
          item
          (get-root-parent parent)))))


;;; TODO: move to new filters mechanism
(defun decode-fullfilter (in-string)
  "Decode fullfilter"
  (when (valid-string-p in-string)
    (let ((*package* (find-package :eshop))
          (tmp (read-from-string in-string)))
      (make-instance 'group-filter
                     :name (getf tmp :name)
                     :base (getf tmp :base)
                     :advanced (getf tmp :advanced)))))
