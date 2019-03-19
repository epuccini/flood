; -------------------------------------------------------------
; Edward Alan Puccini 7.03.2016
; -------------------------------------------------------------
; Crash library for testing
; -------------------------------------------------------------
; file: crash.lisp
; -------------------------------------------------------------
; 
; -------------------------------------------------------------
(in-package :flood)

(defstruct (test-case (:conc-name tc-))
  (name "" :type string)
  (childs '() :type list))

(defmacro report-result (&body forms)
  `(progn
	 ,@(loop for form in forms collect
			`(format *standard-output*
                     "~:[pass~;FAIL~] : ~A : ~A~%"
                     (not ,form) ',form ,form))))

(defgeneric test (object))

(defmethod test ((object test-case))
  (multiple-value-bind (result time)
            (reduce #'(lambda (a b) (and a b))
					(mapcar #'(lambda (unit) (test unit))
							(tc-childs object)))
	(format *standard-output* "~:[FAIL~;pass~] : ~A : ~A~%"
            result (tc-name object) time)
	result))

(defmethod test (fn)
  (multiple-value-bind (result time) (eval fn)
	(format *standard-output* "~:[FAIL~;pass~] : ~A : ~A : ~A~%"
            result fn result time)
	result))

(defmacro test-case (name &body childs)
  "Creates a test-case object with name and test-units in childs."
  `(make-test-case :name ,name :childs (list ,@childs)))


(defmacro define-test-case (name parameter documentation &body body)
  "Create a test-case with error check."
  `(defun ,name ,parameter ,documentation
		  (handler-case
              (let* ((start-time (start-watch))
                     (result (progn ,@body))
                     (stop-time (stop-watch start-time)))
                (values result stop-time))
			(error (condition)
              (format *error-output* "Condition: ~A~%" condition) nil))))
