; -------------------------------------------------------------
; Edward Alan Puccini 19.01.2016
; -------------------------------------------------------------
; Kind of make for common lisp
; -------------------------------------------------------------
; file: make.lisp 
; -------------------------------------------------------------
; make - compile, load and run
; Compile this file and every other needed file gets compiled.
; On error check path in compile-files
; -------------------------------------------------------------
(defparameter *make-config* "")
(defparameter *main-function* nil)
(defparameter *categories* nil)
(defparameter *sources* '())
(defparameter *make-filename* "make.conf")

; -------------------------------------------------------------

(defun load-config ()
  (let ((content 
		 (with-open-file (stream *make-filename* :direction :input)
						 (read stream))))
	content))

(defun build-category (category)
  (declare (keyword category))
  (handler-case 
   (progn
	 (mapc (lambda (file)
			 (format t "Compiling file ~A.~%" file)
			 (load (compile-file file)))
		   (getf *categories* category)) t)
   (error (condition)
		  (format t "Error! ~A." condition))))

(defun build ()
  (loop for category in *categories* do
		(cond ((getf *categories* category) 
			   (format t "Building category ~A.~%" category)
			   (build-category category)))))

#+sbcl 
(defun save-bin (&optional (cmp-flag t))
  (save-lisp-and-die (getf *make-config* :application) 
					 :executable t 
					 :compression cmp-flag 
					 :toplevel *main-function*))

#+clozure
(defun save-bin (&optional (cmp-flag t))
  (declare (ignore cmp-flag))
  (ccl:save-application *application* :toplevel-function *main-function*))

#+ecl 
(defun save-bin (&optional (cmp-flag t))
  (declare (ignore cmp-flag))
  (c:build-program *application*))

(defun build-bin ()
  (build)
  (save-bin))

(defmacro build-files (directory &rest forms)
"Macro for load and compile. Just list some files and they
get compiled and loaded."
  `(progn
    ,@(loop for f in forms collect 
			`(load (compile-file (merge-pathnames ,directory ,f))))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  "Executed at compile time. Does load and compile
all necessary files including packages.
Compile all files on C-c C-k in emacs/slime."

  ;(mapc #'use-package (getf *make-config* :use-packages))

  (print "Make startup...")
  (setq *make-filename* (merge-pathnames 
						 *default-pathname-defaults* "make.conf"))
  (format t "filename '~A' set...~%" *make-filename*)
  (setq *make-config* (load-config))
  (format t "Config read...~%")
  (format t "Building ~A...~%" (getf *make-config* :application))
  (setq *main-function* (getf *make-config* :main))
  (print "Main function found...")
  (setq *categories* (getf *make-config* :categories))
  (mapc #'pprint *categories*)
  (terpri)
  (build)
  (format t "Building  complete. Starting ~A:" (getf *make-config* :application))
  (cond (*main-function*
		 (funcall (symbol-function (find-symbol (string-upcase *main-function*)))))))

