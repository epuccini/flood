; -------------------------------------------------------------
; Edward Alan Puccini 16.01.2016
; -------------------------------------------------------------
; Flood library
; -------------------------------------------------------------
; file: flood.lisp
; -------------------------------------------------------------

(in-package :flood)

(defvar *previous-readtables* '())
(defvar *global-log-level* :dbg)
(defvar *global-config-file* #P"conf/init.conf")
(defvar *global-config* nil)
(defvar *trace-store* (make-hash-table :test 'equal)) ; All trace-functions go in 
										              ; this hash-table
(defvar *history* '())
(defvar *default-logger* nil)

(defvar *day-names*
    '("Monday" "Tuesday" "Wednesday"
      "Thursday" "Friday" "Saturday"
      "Sunday"))

(defstruct logger-type  writers formatter template)
 

;;
;; Time and strings
;;
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

;;
;; File access
;;
(defun copy-file (from to)
  "From is a filepath and to is a filepath.
Binary copy of file is made."
  (with-open-file (in from :direction :input)
    (with-open-file (out to
                        :direction :output
                        :if-exists :overwrite
                        :if-does-not-exist :create)
      (loop for line = (read-line in nil)
         while line do
           (write-line line out)))))
 
(defun move-file (from to)
  "Copy file from to and delete from."
  (copy-file from to)
  (delete-file from))
 
(defun backup-file (from)
  "Create backup-filename and move file to 
backup-location."
  (handler-case
	  (let ((to (concatenate 'string 
							 (getf *global-config* :BACKUP_LOCATION)
							 (getf *global-config* :LOG_FILE_NAME) 
							 "_"
							 (make-day-string)
							 ".log.bak")))
		(move-file from to))
	(error (condition)
	  (write-line (format nil "Error in 'backup-file' ~A" condition)
				  common-lisp:*error-output*))))
 
(defun file-size (filepath)
  "Open file to get file length."
  (with-open-file (stream filepath)
    (file-length stream)))

;;
;; history
;; 
(defun get-history ()
  "Get history. If in async-thread,
then use atomic operation."
  (cond ((equal (bordeaux-threads:current-thread) "async-thread") ; async thread?
        (progn *history*)))
  *history*)
 

(defun set-history (value)
  "Set history with value. If in async thread,
then use atomic operation"
  (cond ((equal (bordeaux-threads:current-thread) "async-thread") ; async thread?
		 (progn (setf *history* value))))
  (setf *history* value))
 
 
(defun append-to-history (entry)
  "Append an entry to history."
  (let ((size (list-length *history*))) ; get size of history
    (cond ((>= size (getf *global-config* :HISTORY_MAX_LINES));  check if we are over size 
		   (pop *history*))) ; pop the first entry
    (set-history (append *history* (list entry)))))
 
 
;;
;; Writers
;;
(defun standard-writer (message)
"Write to standard-stream."
  (write-line message common-lisp:*standard-output*)) 

(defun error-writer (message)
"Write to error-stream."
  (write-line message common-lisp:*error-output*))

(defun check-file-size (filename)
  "Checks the size of a given file and backup
the file if it exceeds LOG_MAX_SIZE in KB."
  (handler-case
	  (let ((filesize (file-size filename)))
		;; check if log exceeds maximum size
		(cond ((> filesize (* 1024 (getf *global-config* :LOG_MAX_SIZE)))
			   (backup-file filename))))
	(error (condition)
	  (write-line (format nil "Error in 'check-file-size' ~A" condition) 
				  *error-output*))))


(defun file-writer (message)
  "Write to file. Apppend or create file."
  (let* ((filename (concatenate 'string 
								(getf *global-config* :LOG_FILE_NAME) ".log")))

	(check-file-size filename)
	(handler-case 
	    (with-open-file (stream filename :direction :output)
		  (write-line message stream))
	  ;; if file exists already then append to file
	  (error ()
		(handler-case
			(with-open-file (stream filename 
									:direction :output
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


;;
;; Utility
;;
(defun format-with-list (fmt-msg args)
  "Dynamic creation of a format-call which
takes a list as parameter."
  (eval
   `(format nil ,fmt-msg ,@args)))


;;
;;  Template
;;
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

;;
;; Formatter
;;
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


;;
;; Logger init and creation
;;
(defun make-logger (&key writers formatter template)
  "A logger consists of writers and formatter and
templates for log-message and -entry."
  (make-logger-type :writers writers
					:template template
					:formatter formatter))

(defun reset-logger ()
  "Create and init logger from config-file."
  (progn
	(setf *global-config* (load-config *global-config-file*))
	(make-logger :writers (mapcar (lambda (f) (symbol-function 
												(find-symbol
												(string-upcase f))))
								  (getf *global-config* :WRITERS))
				 :formatter (symbol-function 
							 (find-symbol
							 (string-upcase 
							  (getf *global-config* :FORMATTER))))
				 :template (getf *global-config* :ENTRY_TEMPLATE))))
  
(defun make-bare-logger (&key writers formatter)
  "Create and init logger."
  (progn
	(make-logger :writers writers
				 :formatter formatter
				 :template (getf *global-config* :ENTRY_TEMPLATE))))
  

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
  (append-to-history (format-with-list fmt-msg args)) ;; history
  (cond ((log-level-p level) ;; log-level fine
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


(defun set-configuration-filepath (filepath)
  "Set configuration filepath with absolute
or relative paths. Side effects."
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


(defun mem (logger fmt-msg &rest fmt-args)
  "Log memory usage by executing the 'room' function."
  (let* ((mem-string (make-memory-usage-string))
		 (log-fmt-msg (concatenate 'string fmt-msg mem-string)))
	(out logger :dbg (format-with-list log-fmt-msg fmt-args))))


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

(defun trace-fn (fn-name logger fmt-msg &rest args)
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
					 (out logger :dbg log-msg))))))) ;; log function msg
	(setf (gethash fn-name *trace-store*) old-fn) ;; store old function via hashes
    (setf (symbol-function 
		   (find-symbol (string-upcase fn-name))) new-fn))) ;; set new function


(defun untrace-fn (fn-name)
  "Set function in fn-name to their old version in *trace-store*"
  (setf (symbol-function (find-symbol (string-upcase fn-name))) 
		(gethash fn-name *trace-store*))
  (remhash fn-name *trace-store*))


(defun stack-out (logger stack-depth fmt-msg &rest args)
  "Use swank to log a stack-trace."
  (let* ((stack-msg (format nil "~A~%"
							(swank-backend:call-with-debugging-environment
							 (lambda () (swank:backtrace 2 (+ stack-depth 2))))))
		 (user-msg (format-with-list fmt-msg args))
		 (log-msg (concatenate 'string user-msg stack-msg)))
  (out logger :dbg log-msg)))


;; 
;; Load config and setup default logger
;;
(setf *global-config* (load-config *global-config-file*))
(setf *default-logger* (reset-logger))


;;
;; Default logging functions
;;
;(setf *default-logger* (reset-logger))

(defun wrn (fmt-msg &rest args)
  (out *default-logger* :prd (format-with-list fmt-msg args)))

(defun inf (fmt-msg &rest args)
  (out *default-logger* :tst (format-with-list fmt-msg args)))

(defun dbg (fmt-msg &rest args)
  (out *default-logger* :dbg (format-with-list fmt-msg args)))

;;
;; Default custom logging functions
;;
(defun cwrn (logger fmt-msg &rest args)
  (out logger :prd (format-with-list fmt-msg args)))

(defun cinf (logger fmt-msg &rest args)
  (out logger :tst (format-with-list fmt-msg args)))

(defun cdbg (logger fmt-msg &rest args)
  (out logger :dbg (format-with-list fmt-msg args)))

;;
;; Async operations
;;
(defun async-prefix (stream char)
  "Reader-macro function for 'async-' substitution."
  (declare (ignore char))
  `(bordeaux-threads:make-thread (lambda () ,(read stream t nil t))))

(defmacro enable-async-syntax ()
  "Enable special-character '°' syntax."
  `(eval-when (:load-toplevel :compile-toplevel :execute)
      (push *readtable* *previous-readtables*)
      (setq *readtable* (copy-readtable))
      (set-macro-character #\° 'async-prefix)))
  
(defmacro disable-async-syntax ()
  "Disable special-character '°' syntax."
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (setq *readtable* (pop *previous-readtables*))))



 
  
