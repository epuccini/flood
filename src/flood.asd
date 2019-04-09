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
  :depends-on ( "usocket" "trivial-features" "bordeaux-threads"
                          "cl-ppcre" "swank" "cl-who" "lparallel") ;; "cl-smtp" "usocket" 
  :components (( :file "package" )
	       ( :file "async-syntax" :depends-on ( "package" ))
	       ( :file "flood" :depends-on ( "async-syntax" ))))
