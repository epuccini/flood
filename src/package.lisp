; -------------------------------------------------------------
; Edward Alan Puccini 16.01.2016
; -------------------------------------------------------------
; Flood package
; -------------------------------------------------------------
; file: package.lisp
; -------------------------------------------------------------

(in-package :cl-user)

(defpackage :flood
  (:use #:cl #:bordeaux-threads #:cl-who #:async-syntax)
  ;;
  ;; All Flood library functions
  ;;
  (:export
   #:*history*
   #:*default-logger*
   #:*terpri*
   #:*server-socket*
   #:*global-config*
   #:start-watch
   #:stop-watch
   #:async
   #:async-prefix
   #:set-kernel
   #:get-cores
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
   #:htmlfile-writer
   #:xmlfile-writer
   #:standard-writer
   #:socket-writer 
   #:rotating-log-writer
   #:error-writer
   #:email-writer
   #:ascii-formatter
   #:html-formatter
   #:xml-formatter
   #:finalize-html
   #:finalize-xml
   #:set-log-level
   #:set-default-logger
   #:make-datetime-string
   #:log-level-p
   #:get-log-level
   #:logger-p
   #:out
   #:capture
   #:capture-ext
   #:exp-log
   #:exp-ext-log
   #:trace-fn
   #:trace-ext-fn
   #:trace-fn
   #:untrace-fn
   #:stack
   #:stack-ext
   #:wrn
   #:inf
   #:dbg
   #:sys
   #:sys-ext
   #:async-out
   #:copy-file-from-to
   #:move-file-from-to
   #:backup-file
   #:file-size
   #:check-file-size
   #:make-string-from-output
   #:make-day-string))
