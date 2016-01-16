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

;; -----------------
;; External packages
;; -----------------

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
  
  (load-and-compile-set *default-pathname-defaults*
	"package.lisp"
	"flood.lisp"))                

;; Example application

(defun main ()
  (let ((lg (flood:create-combined-logger 
			 #'flood:print-logger
			 #'flood:email-logger)))
	(flood:out lg :dbg "Error in ~D function." 666)))

(main)
