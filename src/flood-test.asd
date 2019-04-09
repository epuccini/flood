; -------------------------------------------------------------
; Edward Alan Puccini 07.02.2016
; -------------------------------------------------------------
; Flood library tesing
; -------------------------------------------------------------
; file: flood-test.asd
; -------------------------------------------------------------

(require 'asdf)

(defsystem "flood-test"
  :description "flood a lightweight logger."
  :version "0.1"
  :author "Edward Puccini epuccini@gmx.de"
  :license "LGPL"
  :depends-on ( "trivial-features" "bordeaux-threads" "cl-ppcre" "swank" "optima"
				   "lparallel" "usocket" ) ;;  "usocket" "cl-smtp"
  :components (( :file "package" )
	       ( :file "flood" :depends-on ( "package" ))
	       ( :file "async-syntax" :depends-on ( "flood" ))
	       ( :file "crash" :depends-on ( "async-syntax" ))
	       ( :file "test" :depends-on ("crash") )))
