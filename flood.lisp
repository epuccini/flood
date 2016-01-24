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


(defparameter *global-config-file* #P"conf/init.conf")
(defparameter *global-config* (load-config *global-config-file*))


(defun set-configuration-filepath (filepath)
  "Set configuration filepath with absolute
or relative paths."
  (setq *global-config-file* filepath))


(defun log-level-p (level)
  "Evaluate loglevel and return true or false."
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
  (write-line (format nil fmt args) *error-output*))

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
  (setf *global-config* (load-config *global-config-file*))
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
		   "\\$MACHINE-INSTANCE" format-string (machine-instance)))
	(setf format-string 
		  (cl-ppcre:regex-replace-all 
		   "\\$MACHINE-TYPE" format-string (machine-type)))
	(setf format-string 
		  (cl-ppcre:regex-replace-all 
		   "\\$MACHINE-VERSION" format-string (machine-version)))
	(setf format-string 
		  (cl-ppcre:regex-replace-all 
		   "\\$SOFTWARE-TYPE" format-string (software-type)))
	(setf format-string 
		  (cl-ppcre:regex-replace-all 
		   "\\$SOFTWARE-VERSION" format-string (software-version)))
	(setf format-string 
		  (cl-ppcre:regex-replace-all 
		   "\\$MESSAGE" format-string message-fmt))))


(defun format-args (fmt-msg args)
  "Utility-function to create a formatted
string with list as parameter."
  `(format nil ,fmt-msg ,@args))


(defun out-fn (comb-logger level msg)
  "Simple logging output function'. Calls all 
combinded loggers to create log-entries. No format
strings allowed. Just for use in trace-out and function
where an embedded macro produces a *global-config*
error."
  (cond ((equal (log-level-p level) t)
		 (mapcar (lambda (f) 
				   (funcall f
							(create-format-template 
							 (getf *global-config* :MESSAGE_FORMAT_TEMPLATE)
							 level
							 msg)))
				  comb-logger))))



(defun out (comb-logger level msg-fmt &rest fmt-args)
  "Calls all combinded loggers and creates a log-entries
with a global-format-string, created from the macro
'create-format-template'. Supports format strings with 
given arguments."
  (cond ((equal (log-level-p level) t)
		  (mapcar (lambda (f) 
					(funcall f
							 (eval (format-args 
									(create-format-template 
									 (getf *global-config* :MESSAGE_FORMAT_TEMPLATE)
									 level
									 msg-fmt) fmt-args))))
				  comb-logger))))


(defun set-message-format-template (fmt-str)
  "Change local configuration hash-entry for 
the message-format-template. Reset/Reload with load-config."
  (setf (getf *global-config* :MESSAGE_FORMAT_TEMPLATE) fmt-str)
  fmt-str)


(defmacro with-function-log (comb-logger level msg &rest body)
  "Log function trace and show result. No formatting."
  `(out-fn ,comb-logger
		,level 
		(format nil (concatenate 'string ,msg " ~A = ~{~A ~}") 
				',@body 
				,@body)))


(defun trace-out (fn-name comb-logger level fmt-msg &rest args)
  "Traces a function, but instead of only printing results of
a traced function, all loggers could be used for ouput.
Format strings allowed."
  (let* ((old-fn (symbol-function 
				  (find-symbol (string-upcase fn-name))))
		 (new-fn (lambda (&rest fn-args) 
				   (let* ((result-msg (format nil
											  "#'~A result: ~A." 
											  fn-name
											  (apply old-fn fn-args)))
						  (user-msg (eval (format-args fmt-msg  args)))
						  (new-msg (concatenate 'string user-msg result-msg)))
					 (out-fn comb-logger level new-msg)))))
	(setf (gethash fn-name *trace-store*) old-fn) ;; store old function via hashes
    (setf (symbol-function 
		   (find-symbol (string-upcase fn-name))) new-fn))) ;; set new function


(defun untrace-out (fn-name)
  (setf (symbol-function (find-symbol (string-upcase fn-name))) 
		(gethash fn-name *trace-store*)))




  
