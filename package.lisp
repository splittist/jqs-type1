;;;; jqs-type1

(defpackage #:com.splittist.type1
  (:use #:cl)
  (:export
   #:cff-font
   #:read-cff
   #:make-glyph
   #:with-glyph
   #:move-to
   #:line-to
   #:curve-to
   #:close-path
   ))
