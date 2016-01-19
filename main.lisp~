; -------------------------------------------------------------
; Edward Alan Puccini 16.01.2016
; -------------------------------------------------------------
; Flood logging library make and loader
; -------------------------------------------------------------
; file: make.lisp 
; -------------------------------------------------------------
; make - compile, load and run
; Compile this file and every other needed file gets compiled.
; On error check path in compile-files
; -------------------------------------------------------------
; Requirements: cffi
; -------------------------------------------------------------

(use-package :flood)

(defun main ()
  (let ((lg (flood:create-combined-logger 
			 #'flood:file-logger
			 #'flood:error-logger)))
	(setf flood:*global-log-level* :tst)
	(flood:out lg :dbg "Error in divisian ~D / ~D" 666 555)
	(flood:out lg :tst "Error in multiply ~D * ~D" 666 555)
	(flood:with-trace-log lg :tst 
	  (mapcar (lambda (x) (* x x)) 
			  (append '(1 2 3 4 5) '(4 3 2 1))))))
