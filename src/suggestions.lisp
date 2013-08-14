(in-package #:eshop)

(arnesi:enable-sharp-l-syntax)

(defun sugg.%get-yml-name (product)
  (let ((yml-name (get-option product "Secret" "Yandex")))
    (if (or (null yml-name)
            (string= ""
                     (stripper yml-name))
            (string= "No"
                     (stripper yml-name)))
        (name-seo product)
        yml-name)))

(defun sugg.%get-sugg (sugg query)
  (list :sugg sugg :query query :width (random 1000)))

(defun sugg.%get-art-sugg (art)
  (sugg.%get-sugg art (format nil "art:~A" art)))

(defun sugg.%get-pr-sugg (pr-name)
  (sugg.%get-sugg pr-name (format nil "art:~A" pr-name)))

;; Search query mark
;; Articul art:
;; Name product pr:
;; Name group gr:
;; Name article and static pg:
;; Name vendor vndr:
;; Name filter fltr:
(defun sugg.%get-all-suggestions ()
  (append
   (collect-storage 'product
                    :func #L(sugg.%get-art-sugg (key !1)))
   (collect-storage 'product
                    :func #L(sugg.%get-pr-sugg (sugg.%get-yml-name !1)))))


;; (sugg.%print-to-file #P"/home/wolfor/sugg.bkp")
(defun sugg.%print-to-file (file)
  (with-open-file (stream file
                          :direction :output
                          :if-exists :overwrite
                          :if-does-not-exist :create
                          :external-format :utf-8)
    (mapcar #L(format stream "~A~%" (write-to-string !1))
            (sugg.%get-all-suggestions)))
  t)
