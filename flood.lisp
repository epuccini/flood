; -------------------------------------------------------------
; Edward Alan Puccini 16.01.2016
; -------------------------------------------------------------
; Flood library
; -------------------------------------------------------------
; file: flood.lisp
; -------------------------------------------------------------

(in-package :flood)

(deftype log-level () :dbg :tst :prd)

(defvar *day-names*
    '("Monday" "Tuesday" "Wednesday"
      "Thursday" "Friday" "Saturday"
      "Sunday"))


(defvar *global-log-level* :dbg)
(defvar *global-combined-logger* nil)
(defvar *global-config-file* #P"conf/init.conf")
(defvar *trace-store* (make-hash-table :test 'equal)) ; All trace-functions go in 
									                  ; this hash-table

(defun get-log-level ()
  "Return log level from dynamic var."
  *global-log-level*)

(defun set-log-level (level)
  "Sets the *global-log-level*-symbol with value in 'level'."
  (cond ((or (equal level :dbg) 
			 (equal level :tst) 
			 (equal level :prd))
		 (setq *global-log-level* level)))
  level)


(defun load-config (file)
  (handler-case 
	  (let (store)
		(with-open-file (stream file :direction :input 
								:if-does-not-exist :error)
		  (with-standard-io-syntax
			(setf store (read stream))))
		store)
	(error (condition) 
	  (write-line (format nil "Error in 'load-config': ~A~%" 
						  condition) *error-output*))))


(defvar *global-config* (load-config *global-config-file*))


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


(defun make-datetime-string ()
  "As it says: makes a datetime string from current date and time."
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


(defun time-fn (real-base run-base)
  "Returns an array of two time values
calculated with given start-times."
  (values (float (/ (- (get-internal-real-time) real-base)
			  internal-time-units-per-second))
		  (float (/ (- (get-internal-run-time) run-base)
			 internal-time-units-per-second))))


(defun make-arg-string (fmt-msg args)
  "Utility-function to create a formatted
string with list as parameter."
  (eval
   `(format nil ,fmt-msg ,@args)))
  

(defun standard-logger (fmt &rest args) 
  "Simple standard logger with output to *standard-output*."
   (write-line (format nil fmt args) *standard-output*))

(defun error-logger (fmt &rest args) 
  "Simple error logger with output to *error-output*."
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
		(handler-case
			(with-open-file (stream filename :direction :output
									:if-exists :append)
			  (write-line (format nil fmt args) stream))
		  (error (condition)
			(write-line (format nil "Error in 'file-logger' ~A" condition) 
						*error-output*)))))))


(defmacro init-with-logger (&body args)
  "Initialisation creating a trace-store and sets a list of 
logger which are beeing used in in logging-functions."
  (setf *global-config* (load-config *global-config-file*))
  `(setq *global-combined-logger* 
		 (list ,@args)))


(defun set-logger (logger-list)
  "Set current active logger."
   (setq *global-combined-logger* logger-list))


(defun make-format-template (template level message-fmt)
  "The '*global-format-template*' gets expanded into
a message-format-string. Template-parameters are:
$TIME $LEVEL $MESSAGE $MACHINE-INSTANCE $MACHINE-TYPE
$SOFTWARE-VERSION $SOFTWARE-TYPE and can be used seperatly
or mixed. They will be replaced by corresponding values."
  (let ((format-string ""))
	(setf format-string 
		  (cl-ppcre:regex-replace-all 
						 "\\$TIME" template (make-datetime-string)))
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



(defun out (level msg-fmt &rest fmt-args)
  "Calls all combinded loggers and creates a log-entries
with a global-format-string, created from the macro
'create-format-template'. Supports format strings with 
given arguments."
  (handler-case
	  (cond ((equal (log-level-p level) t)
			 (mapcar (lambda (f) 
					   (funcall f
								(make-arg-string 
								 (make-format-template 
								  (getf *global-config* :MESSAGE_FORMAT_TEMPLATE)
								  level
								  msg-fmt) fmt-args)))
					 *global-combined-logger*)))
	(error (condition) 
	  (write-line (format nil "Error in 'load-config': ~A~%" 
						  condition) *error-output*))))



(defun set-message-format-template (fmt-str)
  "Change local configuration hash-entry for 
the message-format-template. Reset/Reload with load-config."
  (setf (getf *global-config* :MESSAGE_FORMAT_TEMPLATE) fmt-str)
  fmt-str)


(defmacro with-function-log (level msg &rest body)
  "Log function trace and show result. No formatting."
  `(out ,level 
		(format nil (concatenate 'string ,msg " ~A = ~{~A ~}") 
				',@body 
				,@body)))


(defun trace-out (fn-name level fmt-msg &rest args)
  "Traces a function and outputs its results and execution-time.
into configured logger, if any."
  (let* ((real-base (get-internal-real-time)) ; store current times
		 (run-base (get-internal-run-time)) 
		 (old-fn (symbol-function 
				  (find-symbol (string-upcase fn-name))))
		 (new-fn (lambda (&rest fn-args) 
				   (let* ((result-msg (format nil "#'~A ~%Result: ~A~%" fn-name
											  (apply old-fn fn-args)))
						  (user-msg (make-arg-string fmt-msg args)))
					 (multiple-value-bind 
						   (time-real-time time-run-time) (time-fn real-base run-base)
					   (let* ((time-msg (format nil 
												"Execution in real-time ~A s and run-time ~A s" 
												time-real-time time-run-time))
							  (log-msg (concatenate 'string 
													user-msg result-msg time-msg)))
					 (out level log-msg))))))) ;; log function msg
	(setf (gethash fn-name *trace-store*) old-fn) ;; store old function via hashes
    (setf (symbol-function 
		   (find-symbol (string-upcase fn-name))) new-fn))) ;; set new function


(defun untrace-out (fn-name)
  "Set function in fn-name to their old version in *trace-store*"
  (setf (symbol-function (find-symbol (string-upcase fn-name))) 
		(gethash fn-name *trace-store*))
  (remhash fn-name *trace-store*))


(defun stack-out (level stack-depth fmt-msg &rest args)
  "Use swank to log a stack-trace."
  (let* ((stack-msg (format nil "~A~%"
							(swank-backend:call-with-debugging-environment
							 (lambda () (swank:backtrace 2 (+ stack-depth 2))))))
		 (user-msg (make-arg-string fmt-msg args))
		 (log-msg (concatenate 'string user-msg stack-msg)))
  (out level log-msg)))



  
