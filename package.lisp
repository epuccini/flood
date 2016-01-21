; -------------------------------------------------------------
; Edward Alan Puccini 16.01.2016
; -------------------------------------------------------------
; Flood package
; -------------------------------------------------------------
; file: package.lisp
; -------------------------------------------------------------

(in-package :cl-user)

(defpackage :flood
  (:use #:cl #:cl-ppcre) 
  (:export
   :*global-config-file*
   :*global-log-level*
   #:create-date-time-string
   #:create-combined-logger
   #:print-logger
   #:error-logger
   #:file-logger
   #:email-logger
   #:log-level-p
   #:with-trace-log
   #:out))
