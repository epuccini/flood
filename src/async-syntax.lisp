;;
;; Async operations
;;
(in-package :flood)

(defvar *previous-readtables* nil)
(defvar *thread-id* 0)

(defun inc-thread-id ()
  (setq *thread-id* (1+ *thread-id*))
  *thread-id*)
 
(defstruct (times (:conc-name t-))
  (run 0 :type rational)
  (real 0 :type rational))

(defun start-watch ()
  (let ((start-times (make-times)))
    ;; setup start-time
    (setf (t-run start-times) 
          (/ (get-internal-real-time) internal-time-units-per-second))
    (setf (t-real start-times) 
          (/ (get-internal-real-time) internal-time-units-per-second))
    start-times))

(defun stop-watch (start-times)
  (let ((stop-times (make-times))
        (diff-times (make-times)))
    ;; measure stop-times
    (setf (t-run stop-times) 
          (/ (get-internal-real-time) internal-time-units-per-second))
    (setf (t-real stop-times) 
          (/ (get-internal-real-time) internal-time-units-per-second))
    ;; calcualte difference = elapsed time
    (setf (t-run diff-times)
          (- (t-run stop-times) (t-run start-times)))
    (setf (t-real diff-times)
          (- (t-real stop-times) (t-real start-times)))
    diff-times))

(defmacro async (function)
  "Call function in a named thread."
  `(bordeaux-threads:make-thread (lambda () ,function)
                                :name (format nil "async-thread-~D" (inc-thread-id))))

(defun async-fn (function)
  "Call function in a named thread."
  `(bordeaux-threads:make-thread (lambda () ,function)
                                :name (format nil "async-thread-~D" (inc-thread-id))))

(defun async-prefix (stream char)
  "Reader-macro function for 'async-' substitution."
  (declare (ignore char))
  `(bordeaux-threads:make-thread (lambda () 
                                   ,(read stream t nil t))
                                 :name (format nil "async-thread-~D" (inc-thread-id))))

(defun set-kernel (kn)
  "Setup lparallel kernel to the 
desired size."
  (setq lparallel:*kernel* (lparallel:make-kernel kn)))

(defun environment-feature-p (feature)
  "Check environment feature"
  (remove-if-not (lambda (f) (equal feature f)) *features*))

#+darwin
(defun get-cores ()
  "Get number of cores on darwin."
  (let ((cores (subseq (uiop:run-program "sysctl -n hw.ncpu" :output :string) 0 1)))
    (parse-integer cores)))

#+linux
(defun get-cores ()
  "Get number of cores on linux."
  (let ((cores (subseq (uiop:run-program "nproc" :output :string) 0 1)))
    (parse-integer cores)))

#+windows
(defun get-cores ()
  "Get number of cores on windows."
  (let ((cores (subseq (uiop:run-program "echo %NUMBER_OF_PROCESSORS%" :output :string))))
    (parse-integer cores)))

(defmacro async-table (&rest args)
  "Create a table of threads."
  `(progn
     ,@(loop for arg in args collect (async-fn arg))))
