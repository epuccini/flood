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
			  #'flood:error-logger)))
    ;; set logging level
	(setq flood:*global-log-level* :tst)
	(terpri)
	;; simple logging
	(flood:out lg :tst "Error in multiply")
	;; format enabled logging
	(flood:out-fmt lg :dbg "Error in divisian ~D / ~D" 666 555)
	;; trace a function with logging as output
	(flood:trace-out 'test lg :tst "Trace (~D/~D) of function " 1 2)
	(test 20)
	(test 40)
	(test 80)
	;; cleanup and reset to old fn
	(flood:untrace-out 'test)
	;; log a function body
	(flood:with-function-log lg :tst 
	  (mapcar (lambda (x) (* x x)) 
			  (append '(1 2 3 4 5) '(4 3 2 1)))))
  0)
