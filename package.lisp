; -------------------------------------------------------------
; Edward Alan Puccini 16.01.2016
; -------------------------------------------------------------
; Flood package
; -------------------------------------------------------------
; file: package.lisp
; -------------------------------------------------------------

(in-package :cl-user)

(defpackage :flood
  (:use #:cl)
  (:export
   #:*history*
   #:*default-logger*
   #:get-history
   #:history
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
   #:set-default-logger
   #:set-configuration-filepath
   #:make-date-time-string
   #:log-level-p
   #:out
   #:get-log-level
   #:fn-log
   #:ctrace-fn
   #:trace-fn
   #:untrace-fn
   #:stack
   #:cstack
   #:wrn
   #:inf
   #:dbg
   #:cwrn
   #:cinf
   #:cdbg
   #:enable-async-syntax
   #:disable-async-syntax
   #:async-prefix
   #:async-out
   #:mem
   #:cmem
   #:sys
   #:csys))
