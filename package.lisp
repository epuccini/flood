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
   #:set-log-level
   #:set-logger
   #:set-message-format-template
   #:set-configuration-filepath
   #:make-date-time-string
   #:set-logger
   #:init-with-logger
   #:standard-logger
   #:error-logger
   #:file-logger
   #:email-logger
   #:log-level-p
   #:with-function-log
   #:trace-out
   #:untrace-out
   #:stack-out
   #:mem
   #:out))
