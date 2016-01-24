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
   :*global-log-level*
   #:create-date-time-string
   #:init-with-logger
   #:print-logger
   #:error-logger
   #:file-logger
   #:email-logger
   #:log-level-p
   #:with-function-log
   #:trace-out
   #:untrace-out
   #:out))
