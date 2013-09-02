;;;; packages.lisp

(defpackage :search-tips
  (:use :cl)
  (:export :search-tip
           :search-tips
           :build-search-tips
           :max-k-tips-by-prefix
           :tip
           :info
           :tips
           :weight))

;;; TODO: make separated package eshop-core (class-core, slots, backup, etc.)
(restas:define-module #:eshop
  (:use
   ;; system and libs' packages
   :c2cl
   :closure-template
   :anaphora
   :split-sequence
   :cl-ppcre
   :cl-fad
   :alexandria
   ;; :sendmail
   :mongo-cl-driver.sugar ; (son)
   )
  (:shadowing-import-from :cl-fad :copy-stream :copy-file)
  (:export :config.parse-config
           :config.get-option
           :servo.compile-soy
           :xls.update-options-from-xls
           :sklonenie.restore
           :class-core.unserialize-all
           :gateway.load
           :static-pages.restore
           :articles.restore
           :main-page.restore
           :cartrige.restore
           :log-to-this-console))

(defpackage #:eshop.odm
  (:use :c2cl)
  (:export #:serializable-class
           #:serializable-object
           #:serialize
           #:deserialize
           ;; ^^^^ Serialization
           ;; vvvv Persistense
           ;; Nothing here yet though. ;-)
           ))

(in-package #:eshop)
;;; registering classes for proper compilation of methods
;; articles.lisp
(defclass article () ())
;; main-page.lisp
(defclass main-page-storage () ())
(defclass main-page-product () ())
;; xls.lisp
(defclass nko () ())
;; classes.lisp / class-core.lisp
;; TODO: get rid of classes.lisp
(defclass group () ())
(defclass product () ())
(defclass filter () ())
(defclass vendor () ())
(defclass group-filter () ())
;; oneclickcart.lisp
(defclass oneclickcart.answer () ())
;; filters.lisp
(defclass field-filter () ())
;; cartrige.lisp
(defclass printer () ())
