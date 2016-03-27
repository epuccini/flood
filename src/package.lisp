; -------------------------------------------------------------
; Edward Alan Puccini 16.01.2016
; -------------------------------------------------------------
; Flood package
; -------------------------------------------------------------
; file: package.lisp
; -------------------------------------------------------------

(in-package :cl-user)

(defpackage :flood
  (:use #:cl #:bordeaux-threads)
  (:export
   #:*history*
   #:*default-logger*
   #:*terpri*
   #:start-log-server
   #:stop-log-server
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
   #:make-date-time-string
   #:log-level-p
   #:out
   #:get-log-level
   #:exp-log
   #:cexp-log
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
   #:async-out
   #:capture
   #:ccapture
   #:make-string-from-output
   #:sys
   #:csys))
