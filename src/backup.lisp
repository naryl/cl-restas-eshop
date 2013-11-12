;;;;backup.lisp

(in-package #:eshop)

;;; Backup functions and macros

(defmacro backup.define-serialize-method (name class-slots)
  "Macros for creating serialize method"
  `(progn
     (defmethod backup.serialize-entity-to-hashtable ((object ,name))
       (let ((ht (make-hash-table)))
         ,@(mapcar #'(lambda (slot)
                       `(let ((slot-value (,(getf slot :name) object)))
                          (when (slots.%serialize-p
                                 ',(getf slot :type)
                                 ;; initform should be passed unevaluated
                                 slot-value ',(getf slot :initform))
                            (setf (gethash (string',(getf slot :name)) ht)
                                  (slots.%encode-to-string ',(getf slot :type)
                                                           slot-value)))))
                   (remove-if-not #'(lambda (slot)
                                      (getf slot :serialize))
                                  class-slots))
         ht))
     (defmethod backup.serialize-entity ((object ,name))
       (st-json:write-json-to-string (backup.serialize-entity-to-hashtable object)))))

(defun backup.serialize-storage-to-file (type filepath)
  "Serialize storage of given type to file."
  (declare (symbol type) (pathname filepath))
  (with-open-file (file filepath
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create
                        :external-format :utf-8)
    (process-storage #'(lambda (obj)
                         (format file "~A~%" (backup.serialize-entity obj)))
                     type
                     #'serialize-p))
  (log:info "Total serialized: ~A" (count-storage type)))

(defun backup.serialize-storage-to-mongo (type db)
  (declare (symbol type) (mongo:database db))
  (let ((coll (mongo:collection db (string type))))
    (process-storage #'(lambda (obj)
                         (mongo:update-op coll
                                          (son "KEY" (key obj))
                                          (backup.serialize-entity-to-hashtable obj)
                                          :upsert t))
                     type
                     #'serialize-p)))

(defun backup.serialize-all-to-mongo (&optional (db *db*))
  (mapcar #'(lambda (type) (backup.serialize-storage-to-mongo type db))
          '(product filter vendor group)))

(defun backup.serialize-all (&key (backup-dir (config.get-option :paths :path-to-backups))
                             (make-copy (config.get-option :start-options :release))
                             (copy-path
                              (config.get-option :critical :path-to-dropbox-backup)))
  "Serializing all products & groups & vendors to given files in given folder. If no filenames passed, it makes files with type-date-time.bkp template"
  (let* ((date-time (time.encode.backup-filename)))
    (maphash
     #'(lambda (class properties)
         (when (and (getf properties :serialize)
                    (getf properties :storage))
           (let ((path (merge-pathnames
                        (format nil "~(~A~)/~(~:*~A~)-~A.bkp"
                                class date-time) backup-dir)))
             (ensure-directories-exist path)
             (log:info "Start ~(~A~) serialize to ~A" class path)
             (backup.serialize-storage-to-file class path)
             (when make-copy
               (log:info "Making backup copy to ~A" copy-path)
               (ensure-directories-exist copy-path)
               (cl-fad:copy-file
                path (merge-pathnames (format nil "~(~A~).bkp" class) copy-path)
                :overwrite t)))))
     *classes*)))
