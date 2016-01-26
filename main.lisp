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

(in-package :flood)

(defun squares (x)
  (* x x))

(defun main ()
"Small test program"
  (let ((stack-depth 0)
		(lg (init-with-logger 
			  #'file-logger
			  #'error-logger)))
	(terpri)
	;; set global log-level
	(out lg :prd "Swithing to log-level: ~A" 
			   (set-log-level :dbg))
	;; simple logging
	(out lg :dbg "Error in multiply")
	;; format messages logging 
	(out lg :dbg "Error in division ~D / ~D" 666 555)
	;; trace a function and ouput to combined-loggers.
	(trace-out 'squares lg :dbg "Trace fn ")
	(squares 2)
	(squares 4)
	(squares 8)
	;; cleanup and reset to old fn
	(untrace-out 'squares)
	;; log a function body
	(with-function-log lg :dbg "Log function:"
					   (mapcar (lambda (x) (* x x)) 
							   (append '(1 2 3 4 5) '(4 3 2 1))))
	;; setup new format string (in clisp there is a problem with
	;; software-type and -version. They return values, but they are
	;; mixed with data from a different source)"
	(set-message-format-template 
	 "[$MACHINE-TYPE-$SOFTWARE-TYPE]-$TIME-[$LEVEL]-$MESSAGE")
	(out lg :dbg "Testing new format template.")
	;; We are testing log-levels now:
	;; set logging level to DEBUG
	;; set global log-level
	(out lg :prd "Switching to log-level: ~A" 
			   (set-log-level :dbg))
	(out lg :dbg "DEBUG log-output.")
	(out lg :tst "TEST log-output.")
	(out lg :prd "PRODUCTION log-output.")
	;; set logging level to TEST
	;; set global log-level
	(out lg :prd "Switching to log-level: ~A" 
			   (set-log-level :tst))
	(out lg :dbg "DEBUG log-output.")
	(out lg :tst "TEST log-output.")
	(out lg :prd "PRODUCTION log-output.")
	;; set logging level to PRODUCTION
	;; set global log-level
	(out lg :prd "Switching to log-level: ~A" 
			   (set-log-level :prd))
	(out lg :dbg "DEBUG log-output.")
	(out lg :tst "TEST log-output.")
	(out lg :prd "PRODUCTION log-output.")
	;; set global log-level
	(out lg :prd "Swithing to log-level: ~A" 
			   (set-log-level :dbg))
	;; set new loggers
	(let ((new-lg (list  #'standard-logger 
						 #'error-logger 
						 #'file-logger)))
		  ;; log stack trace with depth 3
		  (setq stack-depth 3)
		  (stack-out new-lg :dbg stack-depth 
					 "Stack-trace depth ~D:~%" stack-depth)
	(terpri))))

(main)
