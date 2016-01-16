; -------------------------------------------------------------
; Edward Alan Puccini 16.01.2016
; -------------------------------------------------------------
; Flood package
; -------------------------------------------------------------
; file: package.lisp
; -------------------------------------------------------------

(in-package :common-lisp)

(require 'cffi)

(defpackage :flood
  (:use #:cl) 
  (:export
   #:create-date-time-string
   #:create-combined-logger
   #:print-logger
   #:email-logger
   #:out
   ))
