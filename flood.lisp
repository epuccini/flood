; -------------------------------------------------------------
; Edward Alan Puccini 16.01.2016
; -------------------------------------------------------------
; Flood library
; -------------------------------------------------------------
; file: flood.lisp
; -------------------------------------------------------------

(in-package :flood)

(deftype log-level () :dbg :tst :prd)

(defparameter *day-names*
    '("Monday" "Tuesday" "Wednesday"
      "Thursday" "Friday" "Saturday"
      "Sunday"))

(defparameter *global-log-level* :dbg)
(defparameter *trace-store* (make-hash-table :test 'equal))

(defun load-config (file)
  (handler-case 
	  (let (store)
		(with-open-file (stream file :direction :input 
								:if-does-not-exist :error)
		  (with-standard-io-syntax
			(setf store (read stream))))
		store)
	(error (condition) 
	  (format t "Problem in function 'load-config': ~A~%" condition))))

(defparameter *global-config-file* "conf/init.conf")
(defparameter *global-config* (load-config *global-config-file*));

(defun log-level-p (level)
  "Evaluate loglevel and return true or false."
  (declare (keyword level))
  (cond ((equal level *global-log-level*) t)
		((and (equal level :dbg) (equal *global-log-level* :dbg)) t)
		((and (equal level :dbg) (equal *global-log-level* :tst)) nil)
		((and (equal level :dbg) (equal *global-log-level* :prd)) nil)

		((and (equal level :tst) (equal *global-log-level* :dbg)) t)
		((and (equal level :tst) (equal *global-log-level* :tst)) t)
		((and (equal level :tst) (equal *global-log-level* :prd)) nil)

		((and (equal level :prd) (equal *global-log-level* :dbg)) t)
		((and (equal level :prd) (equal *global-log-level* :tst)) t)
		((and (equal level :prd) (equal *global-log-level* :prd)) t)))

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
  (format t fmt args)
  (terpri))

(defun error-logger (fmt &rest args) 
  "Simple error logger."
  (format *error-output* fmt args)
  (terpri))

(defun email-logger (fmt &rest args)
  "Simple email logger."
  (format t fmt args)
  (terpri))

(defun file-logger (fmt &rest args)
  "Simple rotating file logger."
  (let ((filename (concatenate 'string 
							   (getf *global-config* :LOG_FILE_NAME) ".log")))
	(handler-case 
		(with-open-file (stream filename :direction :output)
		  (write-line (format nil fmt args) stream))
	  (error ()
		(with-open-file (stream filename :direction :output
								:if-exists :append)
		  (write-line (format nil fmt args) stream))))))

(defmacro init-with-logger (&body args)
  "Creates trace-store and creates a list of loggers which 
are beeing used in 'out'-function."
 	(setf *trace-store* (make-hash-table :test #'equal))
	`(list ,@args))

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

(defmacro out (comb-logger level msg-fmt &rest args)
  "Calls all combinded loggers and creates a log-entry 
with a global-format-string, created from the macro
'create-format-template'."
  (declare (keyword level))
  `(cond ((equal (log-level-p ,level) t)
		  (mapcar (lambda (f) 
					(funcall f
							 (format nil 
									 (create-format-template 
									  ,(getf *global-config* :MESSAGE_FORMAT_STRING)
									  ,level
									  ,msg-fmt) ,@args)))
				 ,comb-logger))))

(defmacro with-function-log (comb-logger level &rest body)
  "Log function trace and show result."
  (declare (keyword level))
	`(out ,comb-logger
		  ,level "Trace function: ~A = ~{~A ~}" ',@body ,@body))

(defun trace-out (fn-name comb-logger level)
  (declare (ignore level))
  (let* ((old-fn (symbol-function 
				  (find-symbol (string-upcase fn-name))))
		 (new-fn (lambda (&rest args) 
				   (let ((text (format nil 
									   "Trace of ~A result: ~A" 
									   fn-name
									   (apply old-fn args))))
				   (out comb-logger :tst text nil)))))
	;; store old function via hashes
	(setf (gethash fn-name *trace-store*) old-fn)
	;; set new function
    (setf (symbol-function (find-symbol (string-upcase fn-name))) new-fn)))

(defun untrace-out (fn-name)
  (setf (symbol-function (find-symbol (string-upcase fn-name))) 
		(gethash fn-name *trace-store*)))




  
