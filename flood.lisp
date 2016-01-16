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

(defparameter *day-names*
    '("Monday" "Tuesday" "Wednesday"
      "Thursday" "Friday" "Saturday"
      "Sunday"))

(defparameter *global-log-level* :dbg)
(defparameter *global-format-string* "$TIME--$LEVEL--$MESSAGE")
(defparameter *global-log-file* "FLOOD")
 
(defun create-datetime-string ()
  "As it says: creates a datetime string from current date and time."
  (multiple-value-bind 
		(second minute hour day month year day-of-week dst-p tz)
	  (get-decoded-time)
	(declare (ignore day-of-week dst-p tz))
	(let ((fmt (format nil "~2,'0d.~2,'0d.~2,'0d--~2,'0d:~2,'0d:~4,'0d"
					   hour minute second day month year)))
	  fmt)))

(defun create-day-string ()
  "As it says: creates a day string from current date."
  (multiple-value-bind 
		(second minute hour day month year day-of-week dst-p tz)
	  (get-decoded-time)
	(declare (ignore hour minute second day month year dst-p tz))
	(let ((fmt (format nil "~A" (nth day-of-week *day-names*))))
	  fmt)))

(defun print-logger (fmt &rest args) 
  "Simple console logger."
  (format t fmt args))

(defun error-logger (fmt &rest args) 
  "Simple error logger."
  (format *error-output* fmt args))

(defun email-logger (fmt &rest args)
  "Simple email logger."
  (format t fmt args))

(defun file-logger (fmt &rest args)
  "Simple rotating file logger."
  (let ((filename (concatenate 'string 
							   *global-log-file* "_"
							   (create-day-string) ".log")))
	(handler-case 
		(with-open-file (stream filename :direction :output)
		  (write-line (format nil fmt args) stream))
	  (error ()
		(with-open-file (stream filename :direction :output
								:if-exists :append)
		  (write-line (format nil fmt args) stream))))))

(defun create-combined-logger (&rest args)
  "Create a list of loggers which are beeing used
in 'out'-function."
  (let ((comb '()))
	(mapcar (lambda (f) (push f comb)) args)
	comb))

(defun create-format-template (template level message-fmt)
  "The '*global-format-template*' gets expanded into
a message-format-string. Template-parameter are:
$TIME $LEVEL $MESSAGE"
  (let ((format-string ""))
	(setf format-string 
		  (cl-ppcre:regex-replace-all 
						 "\\$TIME" template (create-datetime-string)))
	(setf format-string 
		  (cl-ppcre:regex-replace-all 
		   "\\$LEVEL" format-string (format nil "~A" level)))
	(setf format-string 
		  (cl-ppcre:regex-replace-all 
		   "\\$MESSAGE" format-string message-fmt))))

(defmacro create-log-string (msg-fmt level args)
  "This macro creates a format-string from a template
'*global-format-string*. This template gets exapanden with
supplied arguments. Output is this string."
  `(format nil (create-format-template 
				,*global-format-string*
				,level
				,msg-fmt) ,@args))

(defmacro out (comb-logger level msg-fmt &rest args)
  "Calls all combinded loggers and creates a log-entry 
with a global-format-string, created from the macro
'create-format-template'."
  `(mapcar (lambda (f) 
			 (funcall f
					  (format nil (create-format-template 
								   ,*global-format-string*
								   ,level
								   ,msg-fmt) ,@args)))
		   ,comb-logger))
