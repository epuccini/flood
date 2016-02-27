; -------------------------------------------------------------
; Edward Alan Puccini 16.01.2016
; -------------------------------------------------------------
; Flood logging library make and loader
; -------------------------------------------------------------
; file: main.lisp 
; -------------------------------------------------------------
; main - example application
; -------------------------------------------------------------
; Requirements: 
; -------------------------------------------------------------

(eval-when (:load-toplevel :compile-toplevel :execute)
  (require 'async-syntax)
  (use-package :async-syntax)
  (use-package :flood))


;; Non-blocking
(enable-async-syntax)

(defun squares (x)
  "Demo function"
  (let ((result 0))
	(loop for cnt from 0 to 100000 do
		 (setf result (+ result (* cnt x))))))

(defun main ()
  ;; Start with default logger. Configured with conf/init.conf
  (wrn  "Hello log! Warning...")
  (inf  "Hello log! Information...")
  (dbg  "Hello log! Debug...")

  ;; init logger with writer and formatter 
  (let ((lg (make-bare-logger 
			 :writers (list #'error-writer #'file-writer)
			 :formatter #'ascii-formatter)))

	;; Set default logger
	(set-default-logger lg)
	(terpri)

	;; simple log output
	(dbg "First custom-log output")

	;; simple log output formatted
	(dbg "Second custom log output with format values: " 666 " inbetween " 999)

	;; trace a function and ouput to combined-loggers.
	(trace-fn 'squares "Trace fn ")

	;; trigger tracing
	(squares 2)
	(squares 4)
	(squares 8)

	;; cleanup and reset to old fn
	(untrace-fn 'squares)

	;; log a function body
	(exp-log :dbg "Log expression:"
					   '(mapcar (lambda (x) (squares x)) 
							   (append '(1 2 3 4 5) '(4 3 2 1))))

	;; setup new format string (in clisp there is a problem with
	;; software-type and -version. They return values, but they are
	;; mixed with data from a different source)"
	
	;; start upd-server
	(start-log-server)

	;; create a custom logger
	(setq lg (make-logger :writers (list #'error-writer #'file-writer #'socket-writer)
						  :formatter #'ascii-formatter
						  :template "[$MACHINE-TYPE]-$TIME-[$LEVEL]-$MESSAGE"))
	;; First output with 
	(cdbg lg "Testing new format template.")

	;; We are testing log-levels now:
	;; set logging level to 
	(cwrn lg "Switching to log-level: " 
		 (set-log-level :dbg))
	(cdbg lg "DEBUG log-output.")
	(cinf lg "INFORMATION log-output.")
	(cwrn lg "WARNING log-output.")

	;; set logging level to TEST
	(cwrn lg "Switching to log-level: " 
		 (set-log-level :tst))
	(cdbg lg "DEBUG log-output.")
	(cinf lg "INFORMATION log-output.")
	(cwrn lg "WARNING log-output.")

	;; set logging level to PRODUCTION
	(cwrn lg "Switching to log-level: " 
		 (set-log-level :prd))
	(cdbg lg "DEBUG log-output.")
	(cinf lg "INFORMATION log-output.")
	(cwrn lg "WARNING log-output.")

	;; set log-level debug
	(set-log-level :dbg)

	;; Back to default logger

	;; just append '°' for marking as async
	°(dbg "Async-out! Debug...")
	°(inf "Async-out! Information...")
	°(wrn "Async-out! Warning...")
	
	;; also for use with expressions
	°(progn (inf "I'm going to sleep...") 
			(sleep 1)
			(inf "Zzzzzzz...")
			(sleep 1)
			(inf "Zzzzzzz...")
			(sleep 1)
			(inf "Zzzzzzz...")
			(sleep 1)
			(inf "I've slept for " 3 " seconds."))
	
	;; set new loggers
	(let ((stack-depth 4))
		  ;; log stack trace with depth 4
		  (stack :dbg stack-depth 
					 "Stack-trace depth: " stack-depth "~%"))
	  
	;; Log memory usage
	(capture :dbg #'room "Memory output:~%")

	;; Load shell command output
	(sys :dbg "ps -e | grep sbcl" "Calling shell-command and log output...~%")

	;; turn off udp-server
	(stop-log-server)))
