;;;; jqs-type1

(in-package #:com.splittist.type1)

;;;# GLYPH

(defclass glyph ()
  ((%font
    :initarg :font
    :accessor glyph-font)
   (%name
    :initarg :name
    :accessor glyph-name)
   (%code-vector
    :initarg :code
    :accessor glyph-code-vector)
   (%width
    :initarg :width
    :accessor glyph-width)))

(defclass type1-glyph (glyph) ;; FIXME name?
  ((%bounding-box
    :initarg :bounding-box
    :accessor glyph-bounding-box)
   (%left-sidebearing
    :initarg :left-sidebearing
    :accessor glyph-left-sidebearing-point)
   ))

(defclass cff-glyph (glyph)
  ((%hstems
    :initarg :hstems
    :accessor glyph-hstems)
   (%vstems
    :initarg :vstems
    :accessor glyph-vstems)
   (%hintmasks
    :initarg :hintmasks
    :accessor glyph-hintmasks)
   (%cntrmasks
    :initarg :cntrmasks
    :accessor glyph-cntrmasks)
   (%randomp
    :initarg :randomp
    :accessor glyph-randomp)
   ))

(defun glyph-stem-count (glyph)
  (+ (length (glyph-hstems glyph))
     (length (glyph-vstems glyph))))
