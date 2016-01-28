; -------------------------------------------------------------
; Edward Alan Puccini 16.01.2016
; -------------------------------------------------------------
; Flood library
; -------------------------------------------------------------
; file: flood.lisp
; -------------------------------------------------------------

(in-package :flood)

(defvar *global-log-level* :dbg)
(defvar *global-config-file* #P"conf/init.conf")
(defvar *global-config* nil)
(defvar *trace-store* (make-hash-table :test 'equal)) ; All trace-functions go in 
									                  ; this hash-table
(defvar *day-names*
    '("Monday" "Tuesday" "Wednesday"
      "Thursday" "Friday" "Saturday"
      "Sunday"))

(defstruct logger-type  writers formatter template)
 

(defun make-day-string ()
  "As it says: creates a day string from current date."
  (multiple-value-bind 
		(second minute hour day month year day-of-week dst-p tz)
	  (get-decoded-time)
	(declare (ignore hour minute second day month year dst-p tz))
	(let ((fmt (format nil "~A" (nth day-of-week *day-names*))))
	  fmt)))


(defun make-datetime-strings ()
  "As it says: makes one date and a time string from current date and time."
  (multiple-value-bind 
		(second minute hour day month year day-of-week dst-p tz)
	  (get-decoded-time)
	(declare (ignore day-of-week dst-p tz))
	(let ((date-fmt (format nil "~2,'0d.~2,'0d.~2,'0d" day month year))
		  (time-fmt (format nil "~2,'0d:~2,'0d:~4,'0d" hour minute second)))
	  (values date-fmt time-fmt))))


(defun standard-writer (message)
"Write to standard-stream."
  (write-line message common-lisp:*standard-output*))
 

(defun error-writer (message)
"Write to error-stream."
  (write-line message common-lisp:*error-output*))

 
(defun file-writer (message)
  "Write to file. Apppend or create file."
  (let ((filename (concatenate 'string 
							   (getf *global-config* :LOG_FILE_NAME) ".log")))
	(handler-case 
		(with-open-file (stream filename :direction :output)
		  (write-line message stream))
	  (error ()
		(handler-case
			(with-open-file (stream filename :direction :output
									:if-exists :append)
			  (write-line message stream))
		  (error (condition)
			(write-line (format nil "Error in 'file-writer' ~A" condition) 
						*error-output*)))))))


(defun rotating-file-writer (message)
"Write to file. When day-change change file-
path and delete files from a week before."
  (let ((filename (concatenate 'string 
							   (getf *global-config* :LOG_FILE_NAME) 
							   "_"
							   (make-day-string)
							   ".log")))
	(handler-case 
		(with-open-file (stream filename :direction :output)
		  (write-line message stream))
	  (error ()
		(handler-case
			(with-open-file (stream filename :direction :output
									:if-exists :append)
			  (write-line message stream))
		  (error (condition)
			(write-line (format nil "Error in 'file-writer' ~A" condition) 
						*error-output*)))))))

 
(defun email-writer (message)
  (print "Not implemented yet!")
  (print message))
 

(defun socket-writer (message)
  (print "Not implemented yet!")
  (print message))


(defun format-with-list (fmt-msg args)
  "Dynamic creation of a format-call which
takes a list as parameter."
  (eval
   `(format nil ,fmt-msg ,@args)))


(defun expand-entry-template (template level message-fmt)
  "Expand entry-template with its parameters.
Template-parameters are:
$DATE $TIME $LEVEL $MESSAGE $MACHINE-INSTANCE $MACHINE-TYPE
$SOFTWARE-VERSION $SOFTWARE-TYPE and can be used seperatly
or mixed. They will be replaced by corresponding values."
  (let ((format-string template))
	(multiple-value-bind (date-fmt time-fmt) (make-datetime-strings)
	  (setf format-string 
			(cl-ppcre:regex-replace-all 
			 "\\$DATE" format-string date-fmt))
	  (setf format-string 
			(cl-ppcre:regex-replace-all 
			 "\\$TIME" format-string time-fmt))
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
			 "\\$MESSAGE" format-string message-fmt)))))


(defun ascii-formatter (writers template level fmt-msg args)
"Just output simple ascii strings within templates."
  (dolist (writer writers)
    (funcall writer
			 (format nil 
					 (expand-entry-template template level fmt-msg) 
					 args))))
 

(defun html-formatter (writers template level fmt-msg args)
  (print "Not implemented yet!")
  (print writers)
  (print template)
  (print level)
  (print fmt-msg)
  (print args))
 

(defun xml-formatter (writers template level fmt-msg args)
  (print "Not implemented yet!")
  (print writers)
  (print template)
  (print level)
  (print fmt-msg)
  (print args))
 

(defun make-logger (&key writers formatter template)
  "A logger consists of writers and formatter and
templates for log-message and -entry."
  (make-logger-type :writers writers
					:template template
					:formatter formatter))


(defun init-logger (&key writers formatter)
  "Create and init logger."
  (setf *global-config* (load-config *global-config-file*))
  (make-logger :writers writers
			   :formatter formatter
			   :template (getf *global-config* :ENTRY_TEMPLATE)))
  

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


(defun out (logger level fmt-msg &rest args)
  "Call formatter with writer and message-template."
  (cond ((log-level-p level)
		 (funcall (logger-type-formatter logger)
				  (logger-type-writers logger)
				  (logger-type-template logger)
				  level
				  fmt-msg
				  args))))


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


(defun time-fn (real-base run-base)
  "Returns an array of two time values
calculated with given start-times."
  (values (float (/ (- (get-internal-real-time) real-base)
			  internal-time-units-per-second))
		  (float (/ (- (get-internal-run-time) run-base)
			 internal-time-units-per-second))))
  

(defun make-memory-usage-string ()
  "Creates a string containing the output of
the 'room' function."
  (with-output-to-string (*standard-output*) (room)))


(defun mem (logger level fmt-msg &rest fmt-args)
  "Log memory usage by executing the 'room' function."
  (let* ((mem-string (make-memory-usage-string))
		 (log-fmt-msg (concatenate 'string fmt-msg mem-string)))
	(out logger level (format-with-list log-fmt-msg fmt-args))))



(defmacro with-function-log (logger level msg &rest body)
  "Log function trace and show result and timing. No formatting."
  (let ((real-base (get-internal-real-time)) ; store current times
		(run-base (get-internal-run-time)))
	(multiple-value-bind 
		  (time-real-time time-run-time) (time-fn real-base run-base)
		`(out ,logger
			  ,level 
			  (format nil 
					  (concatenate 'string ,msg " ~A = ~{~A ~} ~%"
								   "Execution in real-time ~,4f s "
								   "and run-time ~,4f s.") 
					  ',@body 
					  ,@body
					  ,time-real-time
					  ,time-run-time)))))


(defun trace-out (fn-name logger level fmt-msg &rest args)
  "Traces a function and outputs its results and execution-time.
into configured logger, if any."
  (let* ((real-base (get-internal-real-time)) ; store current times
		 (run-base (get-internal-run-time)) 
		 (old-fn (symbol-function 
				  (find-symbol (string-upcase fn-name))))
		 (new-fn (lambda (&rest fn-args) 
				   (let* ((result-msg (format nil "#'~A ~%Result: ~A~%" fn-name
											  (apply old-fn fn-args)))
						  (user-msg (format-with-list fmt-msg args)))
					 (multiple-value-bind 
						   (time-real-time time-run-time) (time-fn real-base run-base)
					   (let* ((time-msg 
							   (format nil 
									   "Execution in real-time ~,4f s and run-time ~,4f s" 
									   time-real-time time-run-time))
							  (log-msg (concatenate 'string 
													user-msg result-msg time-msg)))
					 (out logger level log-msg))))))) ;; log function msg
	(setf (gethash fn-name *trace-store*) old-fn) ;; store old function via hashes
    (setf (symbol-function 
		   (find-symbol (string-upcase fn-name))) new-fn))) ;; set new function


(defun untrace-out (fn-name)
  "Set function in fn-name to their old version in *trace-store*"
  (setf (symbol-function (find-symbol (string-upcase fn-name))) 
		(gethash fn-name *trace-store*))
  (remhash fn-name *trace-store*))


(defun stack-out (logger level stack-depth fmt-msg &rest args)
  "Use swank to log a stack-trace."
  (let* ((stack-msg (format nil "~A~%"
							(swank-backend:call-with-debugging-environment
							 (lambda () (swank:backtrace 2 (+ stack-depth 2))))))
		 (user-msg (format-with-list fmt-msg args))
		 (log-msg (concatenate 'string user-msg stack-msg)))
  (out logger level log-msg)))



  
