; -------------------------------------------------------------
; Edward Alan Puccini 16.01.2016
; -------------------------------------------------------------
; Flood library
; -------------------------------------------------------------
; file: flood.lisp
; -------------------------------------------------------------

(in-package :flood)

;;
;; Requires
;;
(require 'bordeaux-threads)
(require 'cl-ppcre)
(require 'usocket)
(require 'cl-who)

;;
;; Constants and vars
;;
(defvar *global-log-level* :dbg)
(defvar *global-config-file* #P"../conf/flood.conf")
(defvar *global-config* nil)
(defvar *trace-store* (make-hash-table :test 'equal)) ; All trace-functions go in 
										             ; this hash-table
(defvar *server-thread* nil)
(defvar *backup-buffer* "")
(defvar *backup-message* "")

(defvar *history* '())
(defvar *default-logger* nil)
(defvar *terpri* "~%")

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
    (let ((frmt (format nil "~A" (nth day-of-week *day-names*))))
      frmt)))

(defun make-datetime-string ()
  "As it says: makes one date and a time string from current date and time."
  (multiple-value-bind 
        (second minute hour day month year day-of-week dst-p tz)
      (get-decoded-time)
    (declare (ignore day-of-week dst-p tz))
    (let ((date-fmt (format nil "~2,'0d.~2,'0d.~2,'0d" day month year))
          (time-fmt (format nil "~2,'0d:~2,'0d:~2,'0d" hour minute second)))
      (values date-fmt time-fmt))))

(defun make-datetime-string-for-file ()
  "As it says: makes one date and a time string from current date and time."
  (multiple-value-bind 
        (second minute hour day month year day-of-week dst-p tz)
      (get-decoded-time)
    (declare (ignore day-of-week dst-p tz))
    (let ((date-fmt (format nil "~2,'0d.~2,'0d.~2,'0d" day month year))
          (time-fmt (format nil "~2,'0d_~2,'0d_~2,'0d" hour minute second)))
      (values date-fmt time-fmt))))

;;
;; File access
;;
(defun copy-file-from-to (from to)
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
 
(defun move-file-from-to (from to)
  "Copy file from to and delete from."
  (if (probe-file from)
      (progn
    (copy-file-from-to from to)
    (delete-file from))))
 
(defun backup-file (from)
  "Create backup-filename and move file to 
backup-location."
  (handler-case
      (let ((to (concatenate 'string 
                 (getf *global-config* :BACKUP_LOCATION)
                 (getf *global-config* :LOG_FILE_NAME) 
                 ".log.bak")))
        (if (probe-file from) 
        (copy-file-from-to from to)))
    (error (condition)
      (write-line (format nil "Error in 'backup-file' ~A" condition)
                  common-lisp:*error-output*))))
 
(defun file-size (filepath)
  "Open file to get file length."
  (with-open-file (stream filepath)
    (file-length stream)))


(defmacro with-locking (&rest body)
  "Thread safe getting history"
  `(cond ((current-async-thread-p)
          (let ((mutex (make-lock)))
            (acquire-lock mutex)
            (prog1
                ,@body
              (release-lock mutex))))
         ((not (current-async-thread-p))
          (progn
            ,@body))))

;;
;; history
;;
(defun get-history ()
  "Get history. If in async-thread,
then use atomic operation."
  (with-locking
      *history*))

(defun filter (word)
  "Get history and filter by word. If in async-thread,
then use atomic operation."
  (with-locking
      (remove-if-not #'(lambda (entry)
                         (cl-ppcre:scan word entry)) *history*)))

(defun print-history ()
  "Print list of logging entries."
  (with-locking
      (mapc #'print *history*)))

(defun set-history (value)
  "Set history with value. If in async thread,
then use atomic operation"
  (with-locking
      (setf *history* value)))

(defun append-to-history (value)
  "Thread safe append to history."
  (with-locking
      (let ((size (list-length *history*))) ; get size of history
        (progn
          (cond ((>= size (getf *global-config* :HISTORY_MAX_LINES))
                 (pop *history*))) ; pop the first entry
          (setq *history* (append *history* (list value)))))))
         
(defun set-default-logger (logger)
  "Set default formatter, writer and templates."
  (setq *default-logger* logger))

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
   (if (probe-file filename)
      (let ((filesize (file-size filename)))
        ;; check if log exceeds maximum size
        (cond ((> filesize (* 1024 (getf *global-config* :LOG_MAX_SIZE)))
               (backup-file filename)))))
    (error (condition)
      (write-line (format nil "Error in 'check-file-size' ~A" condition) 
                  *error-output*))))


(defun rotating-log-writer (message)
  "Write to file. Apppend or create file."
  (multiple-value-bind (date-fmt) (make-datetime-string-for-file)
    (let* ((filename (concatenate 'string 
                                  (getf *global-config* :LOG_FILE_NAME)
                                  "_" (make-day-string)
                                  "_" date-fmt
                                  ".log")))
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
            (write-line (format nil "Error in 'rotating-log-writer' ~A" condition) 
                        *error-output*))))))))

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

(defun htmlfile-writer (message)
  "Write to file. Apppend or create file."
  (let* ((filename (concatenate 'string 
                                (getf *global-config* :LOG_FILE_NAME) ".html")))

    (check-file-size filename)
    (handler-case 
        (with-open-file (stream filename :direction :output)
          (write-line 
           (with-output-to-string (s) 
             (cl-who:with-html-output (s)
               (:link :rel "stylesheet"
                      :type "text/css"
                      :href "../conf/styles.css")
               (:head (:title "Flood html-log")))) stream)
          (write-line "<body>" stream)
          (write-line message stream))
      ;; if file exists already then append to file
      (error ()
        (handler-case
            (with-open-file (stream filename 
                                    :direction :output
                                    :if-exists :append)
              (write-line message stream))
          (error (condition)
            (write-line (format nil "Error in 'htmlfile-writer' ~A" condition) 
                        *error-output*)))))))

(defun xmlfile-writer (message)
  "Write to file. Apppend or create file."
  (let* ((filename (concatenate 'string 
                                (getf *global-config* :LOG_FILE_NAME) ".xml")))

    (check-file-size filename)
    (handler-case 
        (with-open-file (stream filename :direction :output)
          (write-line "<?xml version='1.0' encoding='ISO-8859-1'?>" stream)
          (write-line "<log>" stream)
          (write-line message stream))
      ;; if file exists already then append to file
      (error ()
        (handler-case
            (with-open-file (stream filename 
                                    :direction :output
                                    :if-exists :append)
              (write-line message stream))
          (error (condition)
            (write-line (format nil "Error in 'xmlfile-writer' ~A" condition) 
                        *error-output*)))))))

(defun finalize-html ()
  "Close html-dile with body tag."
  (multiple-value-bind (date-fmt time-fmt) (make-datetime-string-for-file)
    (let* ((filename (concatenate 'string 
                                  (getf *global-config* :LOG_FILE_NAME) ".html")))
      (handler-case
          (progn
            (with-open-file (stream filename
                                    :direction :output
                                    :if-exists :append)
              (write-line "</body>" stream))
            (move-file-from-to filename
                               (concatenate 'string
                                          (getf *global-config* :LOG_FILE_NAME)
                                          "_" date-fmt
                                          "_" time-fmt
                                          ".html")))
      (error (condition)
        (write-line (format nil "Error in 'finalize-html' ~A" condition) 
                    *error-output*))))))

(defun finalize-xml ()
  "Close html-dile with body tag."
  (multiple-value-bind (date-fmt time-fmt) (make-datetime-string-for-file)
    (let* ((filename (concatenate 'string 
                                  (getf *global-config* :LOG_FILE_NAME) ".xml")))
      (handler-case
          (progn
            (with-open-file (stream filename
                                    :direction :output
                                    :if-exists :append)
              (write-line "</log>" stream))
            (move-file-from-to filename
                               (concatenate 'string
                                            (getf *global-config* :LOG_FILE_NAME)
                                            "_" date-fmt
                                            "_" time-fmt
                                          ".xml")))
        (error (condition)
          (write-line (format nil "Error in 'finalize-xml' ~A" condition) 
                      *error-output*))))))

(defun email-writer (message))
;  (cl-smtp:send-email (getf *global-config* :SMTP_HOST)
;                      (getf *global-config* :EMAIL_FROM)
;                      (getf *global-config* :EMAIL_TO)
;                      "flood-emailer"
;                      (format nil "~A~A~%"
;                              (getf *global-config* :EMAIL_TEMPLATE)
;                              message)
;                      :port (getf *global-config* :EMAIL_PORT)
;                      :authentication '(:login (getf *global-config* :EMAIL_USER)
;                                        (getf *global-config* :EMAIL_PASSWORD))
;                      :ssl (getf *global-config* :EMAIL_SSL)))

#-(or sbcl ccl)
(defun socket-writer (message)
  "Dummy..."
  (format *error-output* "Sockets are not for your lisp..."))

#+(or sbcl ccl)
(defun socket-writer (message)
  "Send message to udp-server."
  (handler-case
      (let* ((server-ip (getf *global-config* :SERVER_IP))
             (port (getf *global-config* :PORT))
             (buffer (concatenate 'string "[SOCKET]>>" message))
             (datagram-socket (usocket:socket-connect server-ip
                                                      port
                                                      :protocol :datagram
                                                      :timeout 10
                                                      :element-type :character)))
        (usocket:socket-send datagram-socket
                             buffer
                             (length buffer)))
    (error (condition)
      (write-line (format nil "Error in 'socket-writer'. ~A~%"
                                         condition) *error-output*))))
;;
;; Utility
;;
(defun format-with-list (fmt-msg args)
  "Dynamic creation of a format-call which
takes a list as parameter."
  (eval
   `(format nil ,fmt-msg ,@args)))

(defun collect-args (args)
  "Create a string out of format-strings
and lambda-lists inbetween."
  (let ((message "")
        (arg-list '()))
    (mapc (lambda (arg)
            (cond ((stringp arg)
                   (setq message (concatenate 'string
                                              message arg)))
                  ((equal (type-of arg) 'cons)
                   (progn
                     (setq message (concatenate 'string 
                                                message "~{~A ~}"))
                     (push `(quote ,arg) arg-list)))
                  ((equal (type-of arg) 'simple-vector)
                   (progn
                     (setq message (concatenate 'string 
                                                message "~{~A ~}"))
                     (push `(quote ,arg) arg-list)))
                  ((not (stringp arg))
                   (progn
                     (setq message (concatenate 'string 
                                                message "~A"))
                     (push arg arg-list))))) args)
    (format-with-list message (reverse arg-list))))

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
    (multiple-value-bind (date-fmt time-fmt) (make-datetime-string)
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
  (let ((message (format nil 
                         (expand-entry-template template level fmt-msg) 
                         args)))
    (append-to-history message)
    (dolist (writer writers)
      (funcall writer message))))
 
(defun one-to-one-formatter (writers template level fmt-msg args)
  "Just output simple ascii strings, plain. Without template."
  (let ((message (format nil 
                         (expand-entry-template template level fmt-msg) 
                         args)))
    (append-to-history message)
    (dolist (writer writers)
      (funcall writer (format-with-list fmt-msg args)))))
 

(defun html-formatter (writers template level fmt-msg args)
  (let* ((message (format nil 
                          (expand-entry-template template level fmt-msg) 
                          args))
         (html-template (getf flood:*global-config* :HTML_TEMPLATE)))
    (append-to-history message)
    (dolist (writer writers)
      (funcall writer
               (expand-entry-template
                html-template level message)))))
 

(defun xml-formatter (writers template level fmt-msg args)
  (let* ((message (format nil 
                          (expand-entry-template template level fmt-msg) 
                          args))
         (xml-template (getf flood:*global-config* :XML_TEMPLATE)))
    (append-to-history message)
    (dolist (writer writers)
      (funcall writer
               (expand-entry-template
                xml-template level message)))))
 


;;
;; Logger init and creation
;;
(defun make-logger (&key writers formatter template)
  "A logger consists of writers and formatter and
templates for log-message and -entry. A custom logger,
is dynamically created and returned with the object."
  (make-logger-type :writers writers
                    :template template
                    :formatter formatter))


(defun make-bare-logger (&key writers formatter)
  "Create and init default logger. Template from config-file."
  (make-logger-type :writers writers
                    :formatter formatter
                    :template (getf *global-config* :ENTRY_TEMPLATE)))
  
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
  

(defun log-level-p (level)
  "Evaluate loglevel and return true or false."
  (cond ((equal level *global-log-level*) t)
        ((and (equal level :dbg) (equal *global-log-level* :dbg)) t)
        ((and (equal level :dbg) (equal *global-log-level* :tst)) t)
        ((and (equal level :dbg) (equal *global-log-level* :prd)) nil)

        ((and (equal level :inf) (equal *global-log-level* :dbg)) t)
        ((and (equal level :inf) (equal *global-log-level* :tst)) t)
        ((and (equal level :inf) (equal *global-log-level* :prd)) t)

        ((and (equal level :wrn) (equal *global-log-level* :dbg)) t)
        ((and (equal level :wrn) (equal *global-log-level* :tst)) t)
        ((and (equal level :wrn) (equal *global-log-level* :prd)) t)))


(defun out (logger level fmt-msg &rest args)
  "Call formatter with writer and use message-template.
Save history"
  (with-locking
      (cond ((log-level-p level) ;; log-level fine
             (funcall (logger-type-formatter logger)
                      (logger-type-writers logger)
                      (logger-type-template logger)
                      level
                      fmt-msg
                      args)))))


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
or relative paths. !SIDE-EFFECTS!."
  (setq *global-config-file* filepath))

(defun function-output-to-string (function)
  "Capture function output and return string."
  (with-output-to-string (*standard-output*) 
    (funcall function)))

(defun make-string-from-command (command)
  "Creates a string containing the output of
the 'room' function."
    (uiop:run-program command :output :string))

;; 
;; Load config and setup default logger
;;
(setf *global-config* (load-config *global-config-file*))
(setf *default-logger* (reset-logger))


(defmacro list-args (&rest args)
  "List args as string"
  `(progn
     ,@(loop for arg in args collect arg)))

(defun logger-p (object)
"Check if variable holds a object object."
  (let* ((logger-type-string (format nil "~A" (type-of object))))
    (string= logger-type-string "LOGGER-TYPE")))

(defun check-logger (obj args)
  "Check if object is a logger, if not append 
object to args and return new args and default logger."
  (cond ((not (logger-p obj))
         (progn
           (push obj args)
           (setq obj *default-logger*))))
  (values obj args))

(defun wrn (arg1 &rest args)
  "Warning-log funection at prd-level with custom-logger if arg1 is a logger. 
Otherwise use *default-logger* and put arg1 to args."
  (multiple-value-bind (logger args) (check-logger arg1 args)
      (out logger :wrn (collect-args args))))

(defun inf (arg1 &rest args)
  "Information-log function at prd-level with custom-logger if arg1 is a logger. 
Otherwise use *default-logger* and put arg1 to args."
  (multiple-value-bind (logger args) (check-logger arg1 args)
      (out logger :inf (collect-args args))))

(defun dbg (arg1 &rest args)
  "Debug-log function at prd-level with custom-logger if arg1 is a logger. Otherwise use
*default-logger* and put arg1 to args."
  (multiple-value-bind (logger args) (check-logger arg1 args)
      (out logger :dbg (collect-args args))))

(defun stack (level stack-depth &rest args)
  "Use swank to log a stack-trace."
  (stack-ext *default-logger* level stack-depth (collect-args args)))

(defun stack-ext (logger level stack-depth &rest args)
  "Use swank to log a stack-trace."
  (let ((trace ""))
    (let* ((msg-lst (remove-if #'null
                               (swank-backend:call-with-debugging-environment
                                (lambda () (swank:backtrace 0 (+ stack-depth 2))))))
           (stack-msg 
            (progn
              (mapcar (lambda (msg)
                        (setf trace (concatenate 'string trace 
                                                   (format nil "~{~A ~}~%" msg))))
                      msg-lst) trace))
           (user-msg (format nil (collect-args args)))
           (log-msg (concatenate 'string user-msg stack-msg)))
      (out logger level log-msg nil))))

(defun exp-log (level msg body)
  "Log with custom logger expression and show result and 
timing. No formatting."
  (exp-ext-log *default-logger* level msg body))

(defun exp-ext-log (logger level msg body)
  "Log with custom logger expression and show result and 
timing. No formatting."
  (let ((local-time (start-watch)))
    (out logger level
         (format nil 
                 (concatenate 'string msg " ~A = ~{~A ~} ~%"
                              "Execution in real-time ~,3f s "
                              "and run-time ~,3f s.") 
                 body
                 (eval body)
                 (t-real (stop-watch local-time))
                 (t-run (stop-watch local-time))))))

(defun trace-fn (fn-name fmt-msg &rest args)
  "Traces a function and log its results and its execution-time."
  (trace-ext-fn *default-logger* fn-name fmt-msg (collect-args args)))

(defun trace-ext-fn (logger fn-name fmt-msg &rest args)
  "Traces a function and log its results and its execution-time.
 Use custom logger."
  (let* ((old-fn (symbol-function 
                  (find-symbol (string-upcase fn-name))))
         (new-fn (lambda (&rest fn-args) 
                   (let* ((local-time (start-watch))
                          (result-msg (format nil 
                                              "#'~A ~%Result: ~A~%" 
                                              fn-name
                                              (apply old-fn fn-args)))
                          (exec-time (stop-watch local-time))
                          (user-msg (format-with-list fmt-msg args))
                          (time-run-time (t-run exec-time))
                          (time-real-time (t-real exec-time))
                          (time-msg 
                           (format nil "Execution in real-time ~,3f s and run-time ~,3f s" 
                                     time-real-time time-run-time))
                          (log-msg (concatenate 'string user-msg result-msg time-msg)))
                     (out logger :dbg log-msg))))) ;; log function msg
    (setf (gethash fn-name *trace-store*) old-fn) ;; store old function via hashes
      (setf (symbol-function 
             (find-symbol (string-upcase fn-name))) new-fn))) ; set new function

(defun untrace-fn (fn-name)
  "Set function in fn-name to their old version in *trace-store*"
  (setf (symbol-function (find-symbol (string-upcase fn-name))) 
        (gethash fn-name *trace-store*))
  (remhash fn-name *trace-store*))

(defun capture (level function &rest args)
  "Capture function-output which is beeing written to 
*standard-output* and log the results."
  (capture-ext *default-logger* level function (collect-args args)))

(defun capture-ext (logger level function &rest args)
  "Capture function-output which is beeing written to 
*standard-output* and log the results. Use custom logger"
  (let* ((mem-string (function-output-to-string function)))
      (out logger level (collect-args (append args (list mem-string))))))

(defun sys (level command &rest args)
  "Capture the output of executed shell-commands 
and log everything."
  (sys-ext *default-logger* level command (collect-args args)))

(defun sys-ext (logger level command &rest args)
  "Capture the output of executed shell-commands 
and log everything. Use custom logger."
  (let* ((command-string (make-string-from-command command)))
    (out logger level (collect-args (append args (list command-string))))))

#+sbcl
(defun udp-handler (buffer)
  "Custom socket handler handles input streams."
  (declare (type (simple-array (unsigned-byte 8) *) buffer))
  (let ((message (map 'string (lambda (x) (code-char x)) buffer))
        (mutex (make-lock)))
    (progn
      (acquire-lock mutex)    
      (if (not (equalp message *backup-message*))
          (funcall 
           #'one-to-one-formatter
           (logger-type-writers *default-logger*)
           (logger-type-template *default-logger*)
           :dbg
           message
           '()))
      (setf *backup-message* message)
      (release-lock mutex))))
 
#-(or sbcl ccl)
(defun start-log-server ()
  "Dummy..."
  (format *error-output* "Sockets are not for your lisp..."))

#+(or sbcl ccl)
(defun start-log-server ()
  "Start upd-server for handling network sent log entries."
  (let ((local-ip (getf *global-config* :LOCAL_IP))
        (port (getf *global-config* :PORT)))
    (setf *server-thread*
          (make-thread 
           #'(lambda ()
               (handler-case
                   (progn
                     (print "Server startup...")
                     (usocket:socket-listen local-ip
                                            port))
                 (error (condition)
                   (write-line (format nil "Error in 'start-log-server. ~A~%"
                                       condition) *error-output*))))))))

#-(or sbcl ccl)
(defun stop-log-server ()
  "Dummy..."
  (format *error-output* "Sockets are not for your lisp..."))

#+(or sbcl ccl)
(defun stop-log-server ()
  "Stop udp-server and reset."
  (destroy-thread *server-thread*))

