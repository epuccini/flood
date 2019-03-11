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
								   "usocket" "cl-smtp" 
								   "async-syntax")
  :components (( :file "package" )
			   ( :file "flood" :depends-on ( "package" ))
			   ( :file "example" :depends-on ( "flood" ))))
