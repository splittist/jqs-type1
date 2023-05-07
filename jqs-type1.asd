;;;; jqs-type1

(defsystem #:jqs-type1
  :description "Access Type 1 font metrics from Common Lisp"
  :author "John Q. Splittist <splittist@splittist.com>"
  :licence "GPL"
  :version "0.0.1"
  :depends-on (#:alexandria
	       #:serapeum
	       #:named-readtables
	       #:trivial-gray-streams
	       )
  :components ((:file "package")
	       (:file "readtable")
	       (:file "utils")
	       (:file "font")
	       (:file "glyph")
	       (:file "charstring")
	       (:file "parse")
	       (:file "interpreter")))
