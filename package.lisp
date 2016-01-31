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
   #:*history*
   #:*default-logger*
   #:copy-file
   #:move-file
   #:file-size
   #:get-history
   #:set-history
   #:append-to-history
   #:async-prefix
   #:make-bare-logger
   #:make-logger
   #:file-writer
   #:standard-writer
   #:socket-writer 
   #:rotating-log-writer
   #:error-writer
   #:ascii-formatter
   #:set-log-level
   #:set-logger
   #:set-configuration-filepath
   #:make-date-time-string
   #:log-level-p
   #:out
   #:get-log-level
   #:with-function-log
   #:trace-fn
   #:untrace-fn
   #:stack-out
   #:wrn
   #:inf
   #:dbg
   #:tst
   #:enable-async-syntax
   #:disable-async-syntax
   #:async-prefix
   #:async-out
   #:mem))
