; -------------------------------------------------------------
; Edward Alan Puccini 16.01.2016
; -------------------------------------------------------------
; Flood package
; -------------------------------------------------------------
; file: package.lisp
; -------------------------------------------------------------

(in-package :common-lisp)

(require 'cl-ppcre)

(defpackage :flood
  (:use #:cl #:cl-ppcre) 
  (:export
   :*global-config-file*
   #:create-date-time-string
   #:create-combined-logger
   #:print-logger
   #:error-logger
   #:file-logger
   #:email-logger
   #:out))
