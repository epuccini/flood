; -------------------------------------------------------------
; Edward Alan Puccini 16.01.2016
; -------------------------------------------------------------
; Flood library
; -------------------------------------------------------------
; file: flood.lisp
; -------------------------------------------------------------

(in-package :flood)

(require 'cl-ppcre)

(deftype log-level () :dbg :rls :prd)

(defparameter *global-log-level* :dbg)
(defparameter *global-format-string* "$TIME--$LEVEL--$MESSAGE")

(defun create-time-string ()
  (multiple-value-bind 
		(second minute hour day month year day-of-week dst-p tz)
	  (get-decoded-time)
	(declare (ignore day-of-week dst-p tz))
	(let ((fmt (format nil "~2,'0d.~2,'0d.~2,'0d--~2,'0d:~2,'0d:~4,'0d"
					   hour minute second day month year)))
	  fmt)))

(defun print-logger (fmt &rest args) 
  (format t fmt args))

(defun email-logger (fmt &rest args)
  (format t fmt args))

(defun create-combined-logger (&rest args)
  (let ((comb '()))
	(mapcar (lambda (f) (push f comb)) args)
	comb))

(defun create-format-string (template level message-fmt)
  (let ((format-string ""))
	(setf format-string 
		  (cl-ppcre:regex-replace-all 
						 "\\$TIME" template (create-time-string)))
	(setf format-string 
		  (cl-ppcre:regex-replace-all 
		   "\\$LEVEL" format-string (format nil "~A" level)))
	(setf format-string 
		  (cl-ppcre:regex-replace-all 
		   "\\$MESSAGE" format-string message-fmt))))

(defun out (comb-logger level msg-fmt &rest args)
  (mapcar (lambda (f) 
			(funcall f (format nil 
							   (concatenate 'string 
											(create-format-string *global-format-string*
																  level
																  msg-fmt) "~%")
							   (create-time-string) 
							   level 
							   args))) comb-logger))
