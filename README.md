 <h1>flood - comfortable tiny logging library for common lisp</h1><br><br>
 
 This library needs "async-syntax" threading support library and "crash" testing support library with "flood" in your asdf-loadpath. Dependencies are "usocket", "cl", "swank", "cl-smtp", "cl-who", "async-syntax" (with "bordeaux-threads", "cffi", "lparallel"), and for testing "crash" (with "optima", "cl-ppcre", "trivial-features"). <br><br>
 This library is thread safe and enables you to log in threads.<br><br>
 To load and use this library, cd into the "src/" directory, start your lisp (testet on sbcl, ecl, ccl)
 and execute:<br><br>
 
	 (asdf:load-system :flood)
 
 If you would like to see and run some examples cd into the "src/" directory, start you lisp and execute:<br><br>
 
	 (asdf:load-system :flood-example)
	 (main)

 Try the following:<br><br>
 
	;;
	;; Start with default logger. Configured with conf/flood.conf<br>
	;; Three simple function make up the core logging facility<br><br>
	;;
	(let ((name "Edward")
	      (level 3))
	;;
	;; just type variables into the places you want them to be
	;;
	(wrn "Hello " name "! Warning-level: " level)
	(inf "Hello " name "! Warning-level: " level)
	(dbg "Hello " name "! Warning-level: " level)

	;; trace demo function 'squares' and send ouput to 
	;; error-writer and file-writer. Use ascii-formatter.
	(trace-fn 'squares "Trace fn ")

	;; trigger tracing
	(squares 2)
	(squares 4)
	(squares 8)

	;; cleanup and reset to old fn
	(untrace-fn 'squares)
  
 	;; create a custom logger with a socket-writer and a new template-string
	;; uncomment socket-writer only if you want to behave as client
	(setq lg (make-logger :writers (list 
					 #'error-writer 
					 #'socket-writer
					 #'file-writer)
					  :formatter #'ascii-formatter<br>
					  :template "[$MACHINE-TYPE]-$TIME-[$LEVEL]-$MESSAGE"))
              
    ;; First output with
    (dbg lg "Testing new format template.")

    ;; stack-trace
    (let ((stack-depth 4))
        ;; log stack trace with depth 4
        (stack :inf stack-depth 
             "Stack-trace depth: " stack-depth "~%"))

    ;; Log memory usage
    (capture :inf #'room "Memory output:~%")

    ;; Load shell command output
    (sys :inf "ps -e | grep sbcl" "Calling shell-command and log output...~%"))

These are the API functions and vars:

	(defpackage :flood
	  (:use #:cl #:bordeaux-threads #:async-syntax)
	  ;;
	  ;; All Flood library functions
	  ;;
	  (:export
	   #:*history*
	   #:*global-config*
	   #:*default-logger*
	   #:*terpri*
	   #:*server-socket*
	   #:start-log-server
	   #:stop-log-server
	   #:get-history
	   #:history
	   #:set-history
	   #:append-to-history
	   #:async-prefix
	   #:make-bare-logger
	   #:make-logger
	   #:file-writer
	   #:standard-writer
	   #:socket-writer 
	   #:rotating-log-writer
	   #:error-writer
	   #:ascii-formatter
	   #:set-log-level
	   #:set-default-logger
	   #:make-datetime-string
	   #:log-level-p
	   #:get-log-level
	   #:logger-p
	   #:out
	   #:capture
	   #:capture-ext
	   #:exp-log
	   #:exp-ext-log
	   #:trace-fn
	   #:trace-ext-fn
	   #:trace-fn
	   #:untrace-fn
	   #:stack
	   #:stack-ext
	   #:wrn
	   #:inf
	   #:dbg
	   #:sys
	   #:sys-ext
	   #:async-out
	   #:copy-file
	   #:move-file
	   #:backup-file
	   #:file-size
	   #:make-string-from-output
	   #:make-day-string))

The following writers are available: 

	   file-writer - writes file with name in configuration specified
	   standard-writer - writes log output to stdout
	   error-writer - writes log output to stderr
	   socket-writer - writes log to host in configuration specified 
	   rotating-log-writer - writes a logfile with day-string in logfilename
	   email-writer - writes every log entry as email (not successfully tested yet. Still pain in cl-smtp)
	   html-writer - writes into html file in configuration specified
	   xml-writer - writes to xml file in configuration specified (to be implemented)
	   
The following formatters are available:

	ascii-formatter - plain ascii format within template
	one-to-one-formatter - plain ascii format without templatE
	html-formatter - html-format
 	xml-formatter - xml-format (to be implemented)
	   
 The configuration file can be found in the "conf/" directory. flood looks for "../conf" when you start at "src/" or "bin/" when you compiled a program.<br><br>
 
 License is based on GNU LESSER GENERAL PUBLIC LICENSE.<br>
 
 
 
 
