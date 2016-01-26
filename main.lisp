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
(defun test (x)
  (* x x))

(defun main ()
  ;; setup logger
  (let ((lg (flood:init-with-logger
			  #'flood:file-logger
			  #'flood:error-logger))
		(stack-depth 0))
    ;; set logging level
	(setq flood:*global-log-level* :tst)
	(terpri)
	;; simple logging
	(flood:out lg :tst "Error in multiply")
	;; format enabled logging
	(flood:out lg :dbg "Error in divisian ~D / ~D" 666 555)
	;; trace a function with logging as output
	(flood:trace-out 'test lg :tst "Trace (~D/~D) of function " 1 2)
	(test 20)
	(test 40)
	(test 80)
	;; cleanup and reset to old fn
	(flood:untrace-out 'test)
	;; log a function body
	(flood:with-function-log lg :tst "Log function:"
							 (mapcar (lambda (x) (* x x)) 
									 (append '(1 2 3 4 5) '(4 3 2 1))))
	;; setup new format string (in clisp there is a problem with
	;; software-type and -version. They return values, but they are
	;; mixed with data from a different source)"
	(flood:set-message-format-template 
	 "[$MACHINE-TYPE-$SOFTWARE-TYPE]-$TIME-[$LEVEL]-$MESSAGE")
	(flood:out lg :tst "Testing new format template.")
	;; log stack trace with depth 4
	(setq stack-depth 4)
	(flood:stack-out lg :tst stack-depth "Stack-trace depth ~D:~%" stack-depth))
  0)
