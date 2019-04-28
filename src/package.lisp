; -------------------------------------------------------------
; Edward Alan Puccini 16.01.2016
; -------------------------------------------------------------
; Flood package
; -------------------------------------------------------------
; file: package.lisp
; -------------------------------------------------------------

(in-package :cl-user)

(defpackage :flood
  (:use #:cl #:bordeaux-threads #:cl-who)
  ;;
  ;; All Flood library functions
  ;;
  (:export
   #:*history*
   #:*default-logger*
   #:*terpri*
   #:*server-socket*
   #:*global-config*
   #:*global-log-level*
   #:filter
   #:current-async-thread-p
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
   #:make-day-string
;; crash
   #:report-result
   #:define-test-case
   #:test
   #:test-case
;; async-syntax
   #:async
   #:times
   #:start-watch
   #:stop-watch
   #:t-run
   #:t-real
   #:enable-async-syntax
   #:disable-async-syntax
   #:set-kernel
   #:environment-feature-p
   #:get-cores
   #:async-table
   #:*thread-id*
   ))
