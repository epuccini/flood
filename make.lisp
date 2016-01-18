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

;
; load and compile helper
;
(defmacro load-and-compile-set (directory &body forms)
"Macro for doing the same things in seuquence"
  `(progn
    ,@(loop for f in forms collect `(load (compile-file (merge-pathnames ,directory ,f))))))

;
; Compile all files on C-c C-c
;
(eval-when (:load-toplevel :compile-toplevel :execute)
  "Executed at compile time. Does load and compile
all necessary files including packages"
  (use-package :flood)

  (load-and-compile-set *default-pathname-defaults*
	"package.lisp"
	"flood.lisp"))                

;; Example application

(defun main ()
  (let ((lg (create-combined-logger 
			 #'file-logger
			 #'error-logger)))
	(setf *global-log-level* :tst)
	(out lg :dbg "Error in divisian ~D / ~D" 666 555)
	(out lg :tst "Error in multiply ~D * ~D" 666 555)
	(with-trace-log lg :tst 
	  (mapcar (lambda (x) (* x x)) 
			  (append '(1 2 3 4 5) '(4 3 2 1))))))

(main)
