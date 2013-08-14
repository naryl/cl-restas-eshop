(require 'asdf)

(defvar *subdir-search-registry* '(#P"~/eshop/libs/")
  "List of directories to search subdirectories within.")
(defvar *subdir-search-wildcard* :wild-inferiors
  "Value of :wild means search only one level of subdirectories; value of :wild-inferiors means search all levels of subdirectories (I don't advise using this in big directories!)")
(defun sysdef-subdir-search (system)
  (let ((latter-path (make-pathname :name (asdf:coerce-name system)
                                    :directory (list :relative
                                                     *subdir-search-wildcard*)
                                    :type "asd"
                                    :version :newest
                                    :case :local)))
    (dolist (d *subdir-search-registry*)
      (let* ((wild-path (merge-pathnames latter-path d))
             (files (directory wild-path)))
        (when files
          (return (first files)))))))

(defun load.register-libs ()
    (pushnew 'sysdef-subdir-search asdf:*system-definition-search-functions*))
