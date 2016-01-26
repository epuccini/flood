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
(defun squares (rng)
  (loop for x from 0 to rng collect
	   (sqrt x)))

(defun main ()
  ;; setup logger
  (let ((lg (flood:init-with-logger
			  #'flood:file-logger
			  #'flood:error-logger))
		(stack-depth 0))
	(terpri)
	;; set global log-level
		;; set global log-level
	(flood:out lg :prd "Swithing to log-level: ~A" 
			   (flood:set-log-level :dbg))
	;; simple logging
	(flood:out lg :dbg "Error in multiply")
	;; format messages logging 
	(flood:out lg :dbg "Error in division ~D / ~D" 666 555)
	;; trace a function and ouput to combined-loggers.
	(flood:trace-out 'squares lg :dbg "Trace fn ")
	(squares 2)
	(squares 4)
	(squares 8)
	;; cleanup and reset to old fn
	(flood:untrace-out 'squares)
	;; log a function body
	(flood:with-function-log lg :dbg "Log function:"
							 (mapcar (lambda (x) (* x x)) 
									 (append '(1 2 3 4 5) '(4 3 2 1))))
	;; setup new format string (in clisp there is a problem with
	;; software-type and -version. They return values, but they are
	;; mixed with data from a different source)"
	(flood:set-message-format-template 
	 "[$MACHINE-TYPE-$SOFTWARE-TYPE]-$TIME-[$LEVEL]-$MESSAGE")
	(flood:out lg :dbg "Testing new format template.")
	;; We are testing log-levels now:
	;; set logging level to DEBUG
		;; set global log-level
	(flood:out lg :prd "Switching to log-level: ~A" 
			   (flood:set-log-level :dbg))
	(flood:out lg :dbg "DEBUG log-output.")
	(flood:out lg :tst "TEST log-output.")
	(flood:out lg :prd "PRODUCTION log-output.")
	;; set logging level to TEST
		;; set global log-level
	(flood:out lg :prd "Switching to log-level: ~A" 
			   (flood:set-log-level :tst))
	(flood:out lg :dbg "DEBUG log-output.")
	(flood:out lg :tst "TEST log-output.")
	(flood:out lg :prd "PRODUCTION log-output.")
	;; set logging level to PRODUCTION
	;; set global log-level
	(flood:out lg :prd "Switching to log-level: ~A" 
			   (flood:set-log-level :prd))
	(flood:out lg :dbg "DEBUG log-output.")
	(flood:out lg :tst "TEST log-output.")
	(flood:out lg :prd "PRODUCTION log-output.")
	;; set global log-level
	(flood:out lg :prd "Swithing to log-level: ~A" 
			   (flood:set-log-level :dbg))
	;; log stack trace with depth 3
	(setq stack-depth 3)
	(flood:stack-out lg :dbg stack-depth "Stack-trace depth ~D:~%" stack-depth)
	(terpri))
  0)
