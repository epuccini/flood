; -------------------------------------------------------------
; Edward Alan Puccini 16.01.2016
; -------------------------------------------------------------
; Flood library
; -------------------------------------------------------------
; file: flood.lisp
; -------------------------------------------------------------

(in-package :flood)

(deftype log-level () :dbg :rls :prd)

(defun create-time-string ()
  (multiple-value-bind 
		(second minute hour day month year day-of-week dst-p tz)
	  (get-decoded-time)
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

(defun out (comb-logger level fmt &rest args)
  (mapcar (lambda (f) 
			(funcall f (format nil (concatenate 'string "~A--~A::" fmt "~%")
							   (create-time-string) 
							   level 
							   args))) comb-logger))
