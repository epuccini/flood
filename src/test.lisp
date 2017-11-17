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

(defun flood-test::main ()
  (test (test-case "flood library"
				   (test-case "flood utility functions"
					 '(test-make-datetime-string)))))

(flood-test:main)
