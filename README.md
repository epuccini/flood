 <h1>flood - comfortable tiny logging library for common lisp</h1><br><br>
 
 This library needs "async-syntax" threading support library and "crash" testing support library with "flood" in your asdf-loadpath. Dependencies are "usocket", "bordeaux-threads", "cl", "async-syntax" and for testing "crash" and "optima". 
 To load and use this library, cd into the "src/" directory, start your lisp (testet on sbcl, ecl, ccl, clisp)
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
	(wrn "Hello log! Warning...")
	(inf "Hello log! Information...")
	(dbg "Hello log! Debug...")

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
    (sys :inf "ps -e | grep sbcl" "Calling shell-command and log output...~%")

  


 The configuration library can be found in the "conf/" directory. flood looks for "../conf" when you start at "src/".<br><br>
 
 License is based on GNU LESSER GENERAL PUBLIC LICENSE.<br>
 
 
 
 
