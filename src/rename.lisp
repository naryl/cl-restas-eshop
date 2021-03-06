;;;; rename.lisp
;;;; set of methods for renamin pictures acctording to SEO names

(in-package #:eshop)

(defgeneric rename.restore-pics-from-backup (product &optional path-to-backup)
  (:documentation "Get pics from backup folder and convert it to working directories"))

(defgeneric rename-remove-product-pics (product)
  (:documentation "Remove allsize pics from images dirs"))

(defparameter *pics-dir-names* (list "big" "goods" "middle" "minigoods" "small")
  "Names of folders for different pics sizes")

(defun rename-new-name (name &optional number)
  "Make image name using SEO name of product"
  (when name
    (setf name
          (ppcre:regex-replace-all                    ; заменяет цепочки подчеркиваний на одно
           "_{2,}"
           (ppcre:regex-replace-all                   ; удаляет оставшиеся непечатные символы и подчеркивания в начале и конце
            "\\W|(_$)|(^_)"
            (ppcre:regex-replace-all                  ; заменяет непечатные символы на подчеркивания
             "\\s|-|/|\\\\|\\[|\\]|\\(|\\)|\\."
             (string-downcase name)
             "_")
            "")
           "_"))
    (let ((result ""))
      (dotimes (i (length name))
        (let ((char (char name i)))
          (setf result (concatenate 'string result (translit-russian-char char)))))
      (if (and number (numberp number))
          (format nil "~a_~2,'0d" result number)
          result))))


(defun rename-check (product)
  "Check picture names for product, return nil if one or more don't match"
  (let ((name (name-seo product))
        (pics (get-pics (key product))))
    (loop
       :for pic :in pics
       :for i :from 1
       :always (equal (rename-new-name name i)
                      (subseq pic 0 (search "." pic :from-end t))))))

(defun rename-in-folder (product folder-path)
  "Rename all pictures in folder using names for product"
  (loop
     :for pic :in (directory (format nil "~a/*.jpg" folder-path))
     :for counter :from 1
     :do (with-open-file (file pic)
           (rename-file file (rename-new-name
                              (name-seo product) counter)))))

(defun rename-product-all-pics (product)
  "Rename all pictures of product"
  (unless (rename-check product)
    (let ((path-art (pics.make-articul-subpath (key product))))
      (loop
         :for folder :in *pics-dir-names*
         :do (rename-in-folder product
                               (format nil "~a/~a/~a" (config.get-option :paths :path-to-pics) folder path-art))))))


(defun rename-force-product-all-pics (product)
  (let ((path-art (pics.make-articul-subpath (key product))))
    (loop
       :for folder :in *pics-dir-names*
       :do (rename-in-folder product
                             (format nil "~a/~a/~a" (config.get-option :paths :path-to-pics) folder path-art)))))


(defun rename-convert-from-folder (product path-to-folder)
  "Get pics from given folder and convert it to five working folders"
  (if (directory-exists-p path-to-folder)
      (let* ((key (key product))
             (name (name-seo product))
             (path-art (pics.make-articul-subpath key)))
        (log:info "Start converting images for product ~a from folder ~a" key path-to-folder)
        (loop
           :for pic :in (directory (format nil "~a/*.jpg" path-to-folder))
           :for counter :from 1
           :do
           (log:info "Converting file ~a" pic)
           (let ((new-name (rename-new-name name counter)))
             (loop
                :for folder :in *pics-dir-names*
                :for size-w :in (list nil 225 200 70 100)
                :for size-h :in (list nil nil 160 70 120)
                :do
                (rename-convert
                 (format nil "~a" pic)
                 (format nil "~a/~a/~a/~a.jpg"
                         (config.get-option :paths :path-to-pics)
                         folder
                         path-art
                         new-name)
                 size-w
                 size-h))
             (log:info "converted to ~a" new-name))))
      (log:warn "Directory ~a doesn't exist!" path-to-folder)))

(defun rename-convert (path-from path-to &optional size-w size-h)
  "Convert picture to given dimensions and put it to given directory"
  (ensure-directories-exist path-to)
  (let* ((size (when size-w
                 (if size-h
                     (format nil "~ax~a" size-w size-h)
                     (format nil "~a" size-w))))
         (proc (sb-ext:run-program "/usr/bin/convert"
                                   (append
                                    (list path-from)
                                    (if size
                                        (append
                                         (list
                                          "-resize"
                                          (format nil "~a\>" size)
                                          "-size"
                                          (format nil "~a" size))
                                         (if size-h
                                             (list
                                              "xc:white"
                                              "+swap"
                                              "-gravity"
                                              "center"
                                              "-composite"))))
                                    (list path-to))
                                   :wait nil :output :stream)))
    (with-open-stream (in (sb-ext:process-output proc))
      (loop
         :for line := (read-line in nil)
         :while line
         :do (log:info line)))))

(defun rename-recursive-get-files (path)
  "Return list of all nested files"
  (let ((result (list (pathname path))))
    (loop
       :for file-or-dir
       :in (list-directory path)
       :do (if (directory-pathname-p file-or-dir)
               (setf result (append result (rename-recursive-get-files file-or-dir)))
               (setf result (append result (list file-or-dir)))))
    result))

(defun rename-copy-folder (from to)
  "Copy folder and all its contents"
  (if (not (directory-exists-p from))
    (log:warn "Directory doesn't exist!")
    ;;else
    (progn
      (ensure-directories-exist to)
      (log:info "Start copy folder ~a to ~a" from to)
      (let ((files-list (rename-recursive-get-files from))
            (len (length from))
            (counter 0))
        (loop
           :for file-or-dir :in files-list
           :do
           (let ((new-to (format nil "~a~a" to (subseq (format nil "~a" file-or-dir) len))))
             (ensure-directories-exist new-to)
             (if (and (not (directory-pathname-p new-to))
                      (not (file-exists-p new-to)))
                 (progn
                   (copy-file file-or-dir new-to)
                   (incf counter))
                 (unless (directory-pathname-p new-to)
                   (log:warn "File ~a already exists!" new-to)))))
        (log:info "Copying finished! ~a files were copied." counter)))))


(defun rename-convert-all (&key (from (merge-pathnames "big-images/" (config.get-option :paths :path-to-dropbox)))
                           (backup (config.get-option :paths :path-to-big-images-backup)))
  "Convert all pictures from folders with articul names"
  (if (and (directory-exists-p from)
           (directory-exists-p backup))
      (progn
        (log:info "Start converting from \"~a\"  Backup folder : \"~a\"" from backup)
        (loop
           :for folder :in (directory (format nil "~a*" from))
           :do (let* ((path (format nil "~a" folder)))
                 (if (directory-exists-p path)
                     (let* ((articul (car (last (split "/" path))))
                            (product (getobj articul 'product)))
                       (when product
                         (rename-convert-from-folder product path)
                         (rename-copy-folder path (format nil "~a~a/" backup articul))
                         (rename-remove-folder path)
                         ;; update pics cache for product
                         (drop-pics-cache (key product))))))))
      ;;else
      (log:warn "Folder ~a or ~a doesn't exist!" from backup)))

(defun rename-remove-folder (path)
  (log:info "Start removing folder ~a" path)
  (let ((proc (sb-ext:run-program "/bin/rm"
                                  (list "-r" path) ; args
                                  :wait nil :output :stream)))
    (with-open-stream (in (sb-ext:process-output proc))
      (loop
         :for line := (read-line in nil)
         :while line
         :do (log:info line))))
  (log:info "Finish removing folder"))

(defmethod rename-remove-product-pics ((product product))
  (let ((path-art (pics.make-articul-subpath (key product))))
    (loop
       :for dir :in *pics-dir-names*
       :do (rename-remove-folder (format nil "~a/~a/~a" (config.get-option :paths :path-to-pics) dir path-art)))))


(defmethod rename-remove-product-pics ((product string))
  (let ((product-object (getobj product 'product)))
    (if product-object
        (rename-remove-product-pics product-object)
        (log:warn "Attempt to remove pics for product with invalid articul ~a" product))))

(defmethod rename.restore-pics-from-backup
    ((product product)
     &optional (path-to-backup
                (config.get-option :paths :path-to-big-images-backup)))
  (when product
    (let* ((articul (format nil "~a" (articul product)))
           (dir-pathname (merge-pathnames articul path-to-backup)))
      (if (directory-exists-p dir-pathname)
          (progn
            (rename-remove-product-pics product)
            (rename-convert-from-folder product dir-pathname))
          (log:warn "Folder with pics for product ~a wasn't found in backup" articul)))))


(defmethod rename.restore-pics-from-backup
    ((product string)
     &optional (path-to-backup
                (config.get-option :paths :path-to-big-images-backup)))
  (let ((product-object (getobj product 'product)))
    (if product-object
        (rename.restore-pics-from-backup product-object path-to-backup)
        (log:warn "Attempt to restore pics for product with invalid articul ~a" product))))
