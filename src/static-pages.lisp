;;;; static-pages.lisp
;;;; Content for static pages

(in-package #:eshop)

(defvar static-pages.*storage* (make-hash-table :test #'equal))

;; загрузка статей из папки
(defun static-pages.process-dir (path &optional (ctype "article"))
  (let ((files))
    (mapcar #'(lambda (x)
                (if (not (cl-fad:directory-pathname-p x))
                    (push x files)))
            (directory (format nil "~a/*.art" path)))
    (mapcar #'(lambda (file)
                (log:info "unserialize static-page ~a" file)
                (unserialize (format nil "~a" file) (make-instance 'article :ctype ctype)))
            files)))


;;заргузка статических страниц из файлов
(defun static-pages.restore ()
  (let ((t-storage))
    (log:info "START:RESTOR:static-pages")
    (let ((*storage-articles* (make-hash-table :test #'equal)))
      (static-pages.process-dir (config.get-option :paths :path-to-static-pages) "static")
      (log:info *storage-articles*)
      (setf t-storage *storage-articles*))
    (setf static-pages.*storage* t-storage)
    (log:info "FINISH:RESTOR:static-pages")))

;;обновление шаблонов для отображения
(defun static-pages.update ()
  (servo.compile-soy "index.soy"))
