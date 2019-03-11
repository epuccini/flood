 ; -------------------------------------------------------------
; Edward Alan Puccini 7.03.2016
; -------------------------------------------------------------
; Flood logging library testing
; -------------------------------------------------------------
; file: test.lisp 
; -------------------------------------------------------------
; test - testing application
; -------------------------------------------------------------
(require 'crash)

(defpackage :flood-test 
  (:use #:cl #:flood #:crash)
  (:export
   #:main
   #:test-all))

(in-package :flood-test)

(defun flood-test::test-make-datetime-string ()
  (multiple-value-bind (date time) (flood:make-datetime-string)
	(let ((res1 (if (= (length date) 10) t nil))
		  (res2 (if (= (length time) 8) t nil)))
	  (and res1 res2))))

(defun flood-test::test-make-day-string ()
  (let ((str (make-day-string)))
	(if (> (length str) 0) t)))

(define-test-case test-copy-file nil "Test if file is copied successfully"
  (flood:copy-file "flood.log" "flood.log.bak")
  (probe-file "flood.log.bak"))

(define-test-case test-move-file nil "Test if file is moved successfully"
  (flood:move-file "flood.log.bak" "flood.log.bak2")
  (probe-file "flood.log.bak2"))

(define-test-case test-file-size nil "Test if file size is read successfully"
  (flood:file-size "flood.log"))

(defun flood-test::main ()
  (test (test-case "flood library"
				   (test-case "flood utility functions"
					 '(test-make-datetime-string)
					 '(test-make-day-string)
					 '(test-copy-file)
					 '(test-move-file)
					 '(test-file-size)))))

(flood-test:main)
