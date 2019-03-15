



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

(defvar *lg* (flood:make-logger :writers (list #'htmlfile-writer
                                               #'file-writer)
                          :formatter #'html-formatter
                          :template "[$MACHINE-TYPE]-$TIME-[$LEVEL]-$MESSAGE"))

(define-test-case test-make-datetime-string nil "Make a datetime string"
  (multiple-value-bind (date time) (flood:make-datetime-string)
    (let ((res1 (if (= (length date) 10) t nil))
          (res2 (if (= (length time) 8) t nil)))
      (and res1 res2))))

(define-test-case test-make-day-string nil "Make a day string"
  (let ((str (make-day-string)))
    (> (length str) 0)))

(define-test-case test-copy-file nil "Test if file is copied successfully"
  (flood:dbg *lg* "Test entry")
  (flood:copy-file-from-to
   (concatenate 'string
                (getf flood:*global-config* :LOG_FILE_NAME)
                ".log")
   (concatenate 'string
                (getf flood:*global-config* :LOG_FILE_NAME) ".log.bak"))
  (probe-file (concatenate 'string
                (getf flood:*global-config* :LOG_FILE_NAME) ".log.bak")))

(define-test-case test-move-file nil "Test if file is moved successfully"
  (flood:move-file-from-to
   (concatenate 'string
                (getf flood:*global-config* :LOG_FILE_NAME)
                ".log.bak")
   (concatenate 'string
                (getf flood:*global-config* :LOG_FILE_NAME)
                ".log.bak2"))
  (probe-file (concatenate 'string
                (getf flood:*global-config* :LOG_FILE_NAME)
                ".log.bak2")))

(define-test-case test-file-size nil "Test if file size is read successfully"
  (flood:dbg *lg* "Test entry")
  (let ((size 
     (flood:file-size
      (concatenate 'string
               (getf flood:*global-config* :LOG_FILE_NAME)
               ".log"))))
    size))

(define-test-case test-backup-file nil "Test if backup of file is successful"
  (flood:dbg *lg* "Test entry")
  (flood:backup-file
   (concatenate 'string
                (getf flood:*global-config* :LOG_FILE_NAME) ".log"))
  (probe-file (concatenate 'string
                (getf flood:*global-config* :BACKUP_LOCATION)
                                (getf flood:*global-config* :LOG_FILE_NAME) 
                ".log.bak")))

(define-test-case test-set-history nil "Test if history is set right"
  (flood:set-history '())
  (not (flood:get-history)))

(define-test-case test-write-log nil "Test if log entries are written"
  (flood:wrn "Test " 1)
  (flood:dbg "Test " 2)
  (flood:inf "Test " 3)
  (probe-file (concatenate 'string
                                (getf flood:*global-config* :LOG_FILE_NAME) ".log")))

(define-test-case test-get-history nil "Test if history is written"
  (let* ((content (flood:get-history))
         (size (length content)))
    (>= size 3)))

(define-test-case test-append-to-history nil "Test if append to history is fine"
  (flood:append-to-history "TEST")
  (>= (length (flood:get-history)) 4))

(define-test-case test-file-writer nil "Test if file writer writes file"
  (let ((lg (flood:make-bare-logger :writers (list #'file-writer)
                                    :formatter #'ascii-formatter))
        (to (concatenate 'string 
                         (getf flood:*global-config* :LOG_FILE_NAME)
                         ".log")))
    (wrn lg "Test")
    (probe-file to)))

(define-test-case test-html-writer nil "Test if htmlfile writer writes htmlfile"
  (let ((lg (flood:make-bare-logger :writers (list #'htmlfile-writer)
                                    :formatter #'ascii-formatter))
        (to (concatenate 'string 
                         (getf flood:*global-config* :HTML_FILE_NAME)
                         ".html")))
    (wrn lg "Test")
    (probe-file to)))

(defun flood-test::main ()
  (test (test-case "flood library"
                   (test-case "flood utility functions"
                     '(test-make-datetime-string)
                     '(test-make-day-string)
                     '(test-file-size)
                     '(test-copy-file)
                     '(test-move-file)
                     '(test-backup-file))
                   (test-case "flood logging functions"
                     '(test-set-history)
                     '(test-write-log)
                     '(test-get-history)
                     '(test-append-to-history)
                     '(test-file-writer)
                     '(test-html-writer)))))

(flood-test:main)
