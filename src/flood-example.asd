; -------------------------------------------------------------
; Edward Alan Puccini 07.02.2016
; -------------------------------------------------------------
; Flood library
; -------------------------------------------------------------
; file: flood.asd
; -------------------------------------------------------------

(require 'asdf)

(defsystem "flood-example"
  :description "flood a lightweight logger. Example app."
  :version "0.1"
  :author "Edward Puccini epuccini@gmx.de"
  :license "LGPL"
  :depends-on ( "trivial-features" "bordeaux-threads" "cl-ppcre" "swank"
				   "cl-who" "lparallel" "usocket") ;; "cl-smtp" "usocket" 
  :components (( :file "package" )
	       ( :file "async-syntax" :depends-on ( "package" ))
	       ( :file "flood" :depends-on ( "async-syntax" ))
	       ( :file "example" :depends-on ( "flood" ))))
