(in-package :st-json)

(defparameter *decode-objects-as* :jso
  "Valid values: :jso :hashtable :alist
  Controls how js objects should be decoded.
  :jso means decode to internal struct which can be processed by getjso, mapjso etc.
  :hashtable means decode as hash tables.
  :alist means decode as association lists")
(export '*decode-objects-as*)

(declaim (function *map-keys*))
(defparameter *map-keys* #'identity)
(export '*map-keys*)

(defparameter *read-null-as* :null)
(export '*read-null-as*)

(defun read-json-object (stream)
  (declare #.*optimize*)
  (ecase *decode-objects-as*
    ((:jso :alist)
     (let ((accum ()))
       (gather-comma-separated
        stream #\} "object literal"
        (lambda ()
          (let ((slot-name (let ((*reading-slot-name* t)) (read-json-element stream))))
            (unless (or (typep slot-name 'string) (typep slot-name 'number))
              (raise 'json-parse-error "Invalid slot name in object literal: ~A" slot-name))
            (skip-whitespace stream)
            (when (not (eql (read-char stream nil) #\:))
              (raise 'json-parse-error "Colon expected after '~a'." slot-name))
            (push (cons (funcall *map-keys* slot-name) (read-json-element stream)) accum))))
       (if (eq *decode-objects-as* :jso)
           (make-jso :alist accum)
           accum)))
     (:hashtable
      (let ((accum (make-hash-table :test 'equal)))
        (gather-comma-separated
         stream #\} "object literal"
         (lambda ()
           (let ((slot-name (let ((*reading-slot-name* t)) (read-json-element stream))))
             (unless (or (typep slot-name 'string) (typep slot-name 'number))
               (raise 'json-parse-error "Invalid slot name in object literal: ~A" slot-name))
             (skip-whitespace stream)
             (when (not (eql (read-char stream nil) #\:))
               (raise 'json-parse-error "Colon expected after '~a'." slot-name))
             (setf (gethash (funcall *map-keys* slot-name) accum) (read-json-element stream)))))
        accum))))

(defmethod write-json-element ((element symbol) stream)
  (declare #.*optimize*)
  (case element
    ((nil) (write-string "[]" stream))
    ((t :true) (write-string "true" stream))
    (:false (write-string "false" stream))
    ((:null :undefined) (write-string "null" stream))
    (t (if (symbolp element)
           (write-string (dash-camel (string element)) stream)
           (raise 'json-write-error "Can not write object of type ~A as JSON." (type-of element))))))

(defun dash-camel (string)
  (let ((bactrian-case (delete #\- (string-capitalize string))))
    (setf (elt bactrian-case 0) (char-downcase (elt bactrian-case 0)))
    bactrian-case))

(defun camel-dash (camel-string)
 (declare (string camel-string))
 (let ((*print-pretty* nil))
   (with-output-to-string (result)
     (loop for c across camel-string
           with last-was-lowercase
           when (and last-was-lowercase
                     (upper-case-p c))
             do (princ "-" result)
           if (lower-case-p c)
             do (setf last-was-lowercase t)
           else
             do (setf last-was-lowercase nil)
           do (princ (char-upcase c) result)))))
(export 'camel-dash)

(defun read-json-atom (stream)
  (declare #.*optimize*)
  (let ((accum (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
    (loop
     (let ((next (peek-char nil stream nil :eof)))
       (when (or (ends-atom next) (eql next :eof))
         (return))
       (vector-push-extend next accum)
       (read-char stream)))
    (let ((number-val (and (looks-like-a-number accum)
                           (ignore-errors (read-from-string accum)))))
      (cond ((numberp number-val) number-val)
            ((string= accum "false") :false)
            ((string= accum "true") :true)
            ((string= accum "null") *read-null-as*)
            ((string= accum "undefined") *read-null-as*)
            ((and *reading-slot-name*
                  (every (lambda (c)
                           (declare (type character c))
                           (or (alphanumericp c) (eql c #\_) (eql c #\$)))
                         accum))
             accum)
            (t (raise 'json-parse-error "Unrecognized value in JSON data: ~A" accum))))))

(defmethod write-json-element ((element hash-table) stream)
  (declare #.*optimize*)
  (write-json-element
   (make-jso :alist (loop :for key :being :the :hash-keys :of element :using (hash-value val)
                          :collect (cons (princ-to-string key) val)))
   stream))
