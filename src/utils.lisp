
(in-package eshop.utils)

;;;; Timers

;; SBCL's timers stop working if one timer fails to return in time.
;; This macro makes it hard to forget to use a timeout :)
(defmacro deftimer ((name interval &optional (timeout 0)) &body body)
  "Defines a function with timeout (you can call it manually) and registers a timer to
execute it each INTERVAL seconds. Use START-TIMER and STOP-TIMER to start and stop it."
  (check-type name symbol)
  `(progn
     (defun ,name ()
       (handler-case
           (sb-ext:with-timeout ,timeout
             ,@body)
         (sb-ext:timeout ()
           (log:error "Timeout in timer ~A" ',name))))
     (defparameter ,name (cons (sb-ext:make-timer #',name)
                               ,interval))))

(defun start-timer (timer)
  (sb-ext:schedule-timer (car timer) (cdr timer)
                         :repeat-interval (cdr timer)))

(defun stop-timer (timer)
  (sb-ext:unschedule-timer (car timer)))

;;;; Time

(defvar *unix-epoch-difference*
  (encode-universal-time 0 0 0 1 1 1970 0))

(defun unix-to-universal-time (unix-time)
  (+ unix-time *unix-epoch-difference*))

(defun universal-to-unix-time (universal-time)
  (- universal-time *unix-epoch-difference*))

(defun get-unix-time (&optional (universal-time (get-universal-time)))
  (universal-to-unix-time universal-time))

(define-constant
    +date+ '(:day "." :month "." :year)
  :test #'equal)

(defun render-time (format &optional (time (get-universal-time)))
  (let ((ts (universal-to-timestamp time)))
    (format-timestring nil ts :format
                       format)))

(defun optional-parameter (name)
  (let ((string (hunchentoot:parameter name)))
    (when (string/= string "")
      string)))

(defmacro with-hunchentoot-parameters ((&rest parameters) &body body)
  "Fetches hunchentoot parameters. Prepend symbol with @ to replace it with NIL if it's
empty. Parameter can be a list to make it look for another parameter name (e.g. when it's
  upcase) like this
  (with-hunchentoot-parameters (param1 @param2 (param3 \"PARAM_3\") ...)"
  `(let ,(loop
            :for param :in parameters
            :collect (let* ((param (ensure-list param))
                            (symbol (first param))
                            (http-param (or (second param)
                                            (string-left-trim '(#\@)
                                                              (string-downcase
                                                               (string (first param)))))))
                       `(,symbol
                         ,(if (char= #\@ (elt (string symbol) 0))
                              `(optional-parameter ,http-param)
                              `(hunchentoot:parameter ,http-param)))))
     ,@body))

;;;; AJAX

(defmacro define-ajax-route (name (template) &body body)
  `(restas:define-route ,name (,template :method :post)
     ,@(remove-if-not #'(lambda (item) (keywordp (car item))) body)
     (st-json:write-json-to-string
      (block ,name
        ,@(remove-if #'(lambda (item) (keywordp (car item))) body)))))

;;;; JSON

(defun write-object-to-json (object)
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (slot (c2cl:class-direct-slots (class-of object)))
      (let ((slot-name (slot-definition-name slot)))
        (setf (gethash (string-downcase (string slot-name)) ht)
              (slot-value object slot-name))))
    (st-json:write-json-to-string ht)))


(defun write-plist-to-json (plist)
  (st-json:write-json-to-string (plist-hash-table plist)))

;;;; Bindings

;;; My version of let+. Sorry.

(defmacro let+ ((&rest clauses) &body body)
  (if clauses
      (let ((clause (car clauses)))
        (nconc (transform-clause clause)
               (if (cdr clauses)
                   (list `(let+ ,(cdr clauses) ,@body))
                   body)))
      `(progn ,@body)))

(defun transform-clause (clause)
  (let ((ignored nil)
        (ignorable nil))
    (flet ((process-vars (vars)
             (mapcar #'(lambda (var)
                         (cond ((equal (symbol-name var) "_")
                                (push (gensym "IGNORED") ignored)
                                (car ignored))
                               ((char= #\_ (elt (symbol-name var) 0))
                                (push var ignorable)
                                var)
                               (t var)))
                     vars))
           (insert-decls ()
             (when (or ignored ignorable)
               (list `(declare ,@(when ignored (list `(ignore ,@ignored)))
                               ,@(when ignorable (list `(ignorable ,@ignorable))))))))
      (cond ((or (symbolp clause) ; let
                 (and (listp clause)
                      (symbolp (car clause))))
             `(let (,clause)))
            ((and (listp clause)  ; destructuring-bind
                  (listp (car clause))
                  (not (keywordp (caar clause))))
             `(destructuring-bind ,(process-vars (car clause))
                  ,(cadr clause)
                ,@(insert-decls)))
            ((and (listp clause)  ; multiple-value-bind
                  (listp (car clause))
                  (eq :values (caar clause)))
             `(multiple-value-bind ,(process-vars (cdar clause))
                  ,(cadr clause)
                ,@(insert-decls)))))))
