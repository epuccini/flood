; -------------------------------------------------------------
; Edward Alan Puccini 07.02.2016
; -------------------------------------------------------------
; Flood library
; -------------------------------------------------------------
; file: flood.asd
; -------------------------------------------------------------

(require 'asdf)

(defsystem "flood"
  :description "flood a lightweight logger."
  :version "0.1"
  :author "Edward Puccini epuccini@gmx.de"
  :license "LGPL"
  :depends-on ( "trivial-shell" "bordeaux-threads" "cl-ppcre" "swank" "async-syntax")
  :components (( :file "package" )
			   ( :file "flood" :depends-on ( "package" ))))
;;			   ( :file "example" :depends-on ( "flood" ))))
