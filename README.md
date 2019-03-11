 <h1>flood - comfortable tiny logging library for common lisp</h1><br><br>
 
 This library needs "async-syntax" threading support library with "flood" in your asdf-loadpath.
 Dependencies are "usocket", "bordeaux-threads", "cl" and "async-syntax". 
 To load and use this library, cd into the "src/" directory, start your lisp (testet on sbcl, ecl, ccl, clisp)
 and execute:<br><br>
 
	 (asdf:load-system :flood)<br><br>
 
 If you would like to see and run some examples cd into the "src/" directory, start you lisp and execute:<br><br>
 
	 (asdf:load-system :flood-example)<br>
	 (main)<br><br>

 Try the following:<br><br>
 
	;;<br>
	;; Start with default logger. Configured with conf/flood.conf<br>
	;; Three simple function make up the core logging facility<br><br>
	;;
	(wrn "Hello log! Warning...")<br>
	(inf "Hello log! Information...")<br>
	(dbg "Hello log! Debug...")<br><br>

	;; trace demo function 'squares' and send ouput to <br>
	;; error-writer and file-writer. Use ascii-formatter.<br>
	(trace-fn 'squares "Trace fn ")<br><br>

	;; trigger tracing<br>
	(squares 2)<br>
	(squares 4)<br>
	(squares 8)<br><br>

	;; cleanup and reset to old fn<br>
	(untrace-fn 'squares)<br><br>
  
 	;; create a custom logger with a socket-writer and a new template-string<br>
	;; uncomment socket-writer only if you want to behave as client<br>
	(setq lg (make-logger :writers (list 
					 #'error-writer <br>
					 #'socket-writer<br>
					 #'file-writer)<br>
					  :formatter #'ascii-formatter<br>
					  :template "[$MACHINE-TYPE]-$TIME-[$LEVEL]-$MESSAGE"))<br><br>
              
    ;; First output with <br>
    (dbg lg "Testing new format template.")<br><br>

    ;; stack-trace<br>
    (let ((stack-depth 4))<br>
        ;; log stack trace with depth 4<br>
        (stack :inf stack-depth <br>
             "Stack-trace depth: " stack-depth "~%"))<br><br>

    ;; Log memory usage<br>
    (capture :inf #'room "Memory output:~%")<br><br>

    ;; Load shell command output<br>
    (sys :inf "ps -e | grep sbcl" "Calling shell-command and log output...~%")<br><br>

  


 The configuration library can be found in the "conf/" directory. flood looks for "../conf" when you start at "src/".<br><br>
 
 License is based on GNU LESSER GENERAL PUBLIC LICENSE.<br>
 
 
 
 
