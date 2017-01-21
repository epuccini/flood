; -------------------------------------------------------------
; Edward Alan Puccini 7.03.2016
; -------------------------------------------------------------
; Flood logging library testing
; -------------------------------------------------------------
; file: test.lisp 
; -------------------------------------------------------------
; test - testing application
; -------------------------------------------------------------

(defpackage :flood-test 
  (:use #:cl #:flood)) ;; #:crash))

(in-package :flood-test)

(defun flood-test::test-make-datetime-strings ()
  )

(defun flood-test::test-all ()
  (test-make-datetime-strings))

(test-all)
