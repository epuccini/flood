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
(defparameter *make-conf* "")
(defparameter *main-function* nil)
(defparameter *categories* nil)
(defparameter *sources* '())
(defparameter *make-filename* "make.conf")

; -------------------------------------------------------------

(defmacro build-files (directory &rest forms)
"Macro for load and compile. Just list some files and they
get compiled and loaded."
  `(progn
    ,@(loop for f in forms collect 
			`(load (compile-file (merge-pathnames ,directory ,f))))))
;; Example:
;; (build-files *default-pathname-defaults*
;;              "example/test1.lisp"
;;              "example/main.lisp")

(defun quickload-list (systems)
"Macro to quickload a system."
  (mapc #'ql:quickload systems)
  (print "Systems loaded..."))


(defun require-list (packages)
  "Function require a list of packages."
  (print "Start require packages")
  (mapc #'require packages))


(defun load-config ()
  (let ((content 
		 (with-open-file (stream *make-filename* :direction :input)
						 (read stream))))
	content))

(defun setup ()
  "This function has side effects and sets the
globals with valid configuration properties."
  (setq *make-filename* (merge-pathnames 
						 *default-pathname-defaults* "make.conf"))
  (format t "filename '~A' set...~%" *make-filename*)
  (setq *make-conf* (load-config))
  (format t "Config read...~%")
  (setq *categories* (getf *make-conf* :categories))
  (mapc #'pprint *categories*)
  (setq *main-function* (getf *make-conf* :main))
  (setq *application* (getf *make-conf* :application))
  (terpri))
  

(defun cbuild (category)
  "Load and compile all files in given
category."
  (declare (keyword category))
  (handler-case 
   (progn
	 (mapc (lambda (file)
			 (format t "Compiling file ~A.~%" file)
			 (load (compile-file file)))
		   (getf *categories* category)) t)
   (error (condition)
		  (format t "Error in cbuild! ~A." condition))))

(defun build ()
  "Load and compile all files in all categories."
  (handler-case 
	  (progn
		(format t "Building ~A...~%" *application*)
		(loop for category in *categories* do
			 (cond ((getf *categories* category) 
					(format t "Building category ~A.~%" category)
					(cbuild category)))))
	(error (condition)
	  (format t "Error in build! ~A." condition))))

	   
#+sbcl 
(defun save-bin ()
  "Save binary-file with sbcl."
  (handler-case
	  (save-lisp-and-die  *application*
						  :executable t 
						  :compression t 
						  :toplevel *main-function*)
	(error (condition)
	  (format t "Error in save-bin! ~A." condition))))

#+clozure
(defun save-bin ()
  "Save binary-file with clozure-lisp."
  (handler-case
	  (progn
		;(require 'ccl)
		;(require :build-application)
		;;(require 'cocoa)
		 (ccl:save-application *application*
							   :purify t
							   :prepend-kernel t
							   :application-class 'ccl::application
							   :toplevel-function *main-function*))
	(error (condition)
	  (format t "Error in save-bin! ~A." condition))))

#+ecl 
(defun save-bin ()
  "Save binary-file with ecl."
  (handler-case
	  (c:build-program *application*)
	(error (condition)
	  (format t "Error in save-bin! ~A." condition))))


#+clisp 
(defun save-bin ()
  "Save binary-image with clisp."
  (handler-case
	  (ext:saveinitmem *application* 
					   :executable t 
					   :verbose t 
					   :init-function *main-function*)
	(error (condition)
	  (format t "Error in save-bin! ~A." condition))))

(defun run ()
  (handler-case
	  (cond (*main-function*
		 (funcall (symbol-function 
				   (find-symbol (string-upcase *main-function*))))))
	(error (condition)
	  (format t "Error! ~A~%" condition))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  "Executed at compile time. Does load and compile
all necessary files including packages.
Compile all files on C-c C-k in emacs/slime"

  (print "Make startup...")
  ;; Load configuration
  (setup)
  ;; Quickload systems
  (quickload-list (getf *make-conf* :systems))
  ;; Require packages
  (require-list (getf *make-conf* :packages))
  ;; compile everything
  (build)
  (format t "Building  complete. Starting ~A:" *application*)
  (run))

