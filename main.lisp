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
  (use-package :flood))


;; Non-blocking
(enable-async-syntax)


(defun squares (x)
  (* x x))

(defun main ()
  ;; Start with default logger. Configured with conf/init.conf
  (wrn  "Hello log! Warning...")
  (dbg  "Hello log! Debug...")

  ;; init custom logger with writer and formatter 
  (let ((lg (make-bare-logger 
			 :writers (list #'standard-writer #'file-writer)
			 :formatter #'ascii-formatter)))
	(terpri)
	;; simple log output
	(out lg :dbg "First custom-log output")

	;; simple log output formatted
	(out lg :dbg "Second custom log output with format values:~A." 666)

	;; just append '°' for marking as async
	°(wrn  "Async-out! Warning...")
	°(dbg  "Async-out! Debug...")

	;; also for use with expressions
	°(progn (wrn "I'm going to sleep...") 
			(sleep 5)
			(inf "I've slept for ~D seconds." 5))

	;; trace a function and ouput to combined-loggers.
	(trace-fn 'squares lg "Trace fn ")

	;; trigger tracing
	(squares 2)
	(squares 4)
	(squares 8)

	;; cleanup and reset to old fn
	(untrace-fn 'squares)

	;; log a function body
	(with-function-log lg :dbg "Log function:"
					   (mapcar (lambda (x) (* x x)) 
							   (append '(1 2 3 4 5) '(4 3 2 1))))

	;; setup new format string (in clisp there is a problem with
	;; software-type and -version. They return values, but they are
	;; mixed with data from a different source)"
	
	;; create a new logger
	(setq lg (make-logger :writers (list #'error-writer  #'file-writer)
						  :formatter #'ascii-formatter
						  :template "[$MACHINE-TYPE]-$TIME-[$LEVEL]-$MESSAGE"))
	(out lg :dbg "Testing new format template.")

	;; We are testing log-levels now:
	;; set logging level to 
	(out lg :prd "Switching to log-level: ~A" 
		 (set-log-level :dbg))
	(out lg :dbg "DEBUG log-output.")
	(out lg :tst "TEST log-output.")
	(out lg :prd "PRODUCTION log-output.")

	;; set logging level to TEST
	(out lg :prd "Switching to log-level: ~A" 
		 (set-log-level :tst))
	(out lg :dbg "DEBUG log-output.")
	(out lg :tst "TEST log-output.")
	(out lg :prd "PRODUCTION log-output.")

	;; set logging level to PRODUCTION
	(out lg :prd "Switching to log-level: ~A" 
		 (set-log-level :prd))
	(out lg :dbg "DEBUG log-output.")
	(out lg :tst "TEST log-output.")
	(out lg :prd "PRODUCTION log-output.")

	;; set log-level debug
	(set-log-level :dbg)

	;; set new loggers
	(let ((stack-depth 4))
		  ;; log stack trace with depth 4
		  (stack-out lg :dbg stack-depth 
					 "Stack-trace depth ~D:~%" stack-depth))
		  
	;; Output memory usage
	(mem lg :dbg "Memory output:~%")))
