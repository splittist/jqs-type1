;;;; jqs-type1

(in-package #:com.splittist.type1)

;;;# CFF

(defun read-type2-operand (stream b0)
  (cond
    ((= 28 b0)
     (let ((result (read-card16 stream)))
       (if (logbitp 15 result)
	   (1- (- (logandc2 #xFFFF result)))
	   result)))
    ((<= 32 b0 246)
     (- b0 139))
    ((<= 247 b0 250)
     (let ((b1 (read-byte stream)))
       (+ (* 256 (- b0 247)) b1 108)))
    ((<= 251 b0 254)
     (let ((b1 (read-byte stream)))
       (- (* -256 (- b0 251)) b1 108)))
    ((= 255 b0)
     (read-16.16 stream))))

;;;# PS-CONTEXT

(defclass ps-context ()
  ((%font
    :initarg :font
    :reader ps-context-font)
   (%glyph
    :initarg :glyph
    :reader ps-context-glyph)
   (%stack
    :initform (make-array 48 :fill-pointer 0)
    :accessor ps-context-stack)
   (%current-x
    :initform 0
    :accessor ps-context-current-x)
   (%current-y
    :initform 0
    :accessor ps-context-current-y)
   (%path-x
    :initform 0
    :accessor ps-context-path-x)
   (%path-y
    :initform 0
    :accessor ps-context-path-y)
   ))

(defclass cff-context (ps-context)
  ((%transient-array
    :initform (make-array 32)
    :accessor cff-context-transient-array)
   (%hintmask-index
    :initform 0
    :accessor cff-context-hintmask-index)))

(defparameter *context* nil)
 
(defmacro ps-stack (&optional index)
  (if index
      `(aref (ps-context-stack *context*) ,index)
      '(ps-context-stack *context*)))

(defmacro ps-current-y ()
  '(ps-context-current-y *context*))

(defmacro ps-current-x ()
  '(ps-context-current-x *context*))

(defmacro ps-path-x ()
  '(ps-context-path-x *context*))

(defmacro ps-path-y ()
  '(ps-context-path-y *context*))

(defmacro ps-transient-array ()
  '(cff-context-transient-array *context*))

(defmacro ps-hintmask-index ()
  '(cff-context-hintmask-index *context*))

(defun ps-push (element)
  (vector-push element (ps-stack)))

(defun ps-pop ()
  (vector-pop (ps-stack)))

(defun ps-clear-stack ()
  (setf (fill-pointer (ps-stack)) 0))

(defun ps-subroutine (num)
  (cff-subroutine (ps-context-font *context*) num))

(defun ps-global-subroutine (num)
  (cff-global-subroutine (ps-context-font *context*) num))

;;;# Path Primitives

;;;# 4.1 Path Construction Operators

(defun ps-rmoveto (start)
  ;;  dx1 dy1
  (lambda ()
    (unless (and (= (ps-current-x) (ps-path-x))
		 (= (ps-current-y) (ps-path-y)))
      (line (ps-path-x) (ps-path-y)))
    (move (incf (ps-current-x) (ps-stack start))
	  (incf (ps-current-y) (ps-stack (1+ start))))
    (setf (ps-path-x) (ps-current-x)
	  (ps-path-y) (ps-current-y))
    (ps-clear-stack)))

(defun ps-hmoveto (start)
  ;; dx1
  (lambda ()
    (unless (and (= (ps-current-x) (ps-path-x))
		 (= (ps-current-y) (ps-path-y)))
      (line (ps-path-x) (ps-path-y)))
    (move (incf (ps-current-x) (ps-stack start))
	  (ps-current-y))
    (setf (ps-path-x) (ps-current-x)
	  (ps-path-y) (ps-current-y))
    (ps-clear-stack)))

(defun ps-vmoveto (start)
  ;; dy1
  (lambda ()
    (unless (and (= (ps-current-x) (ps-path-x))
		 (= (ps-current-y) (ps-path-y)))
      (line (ps-path-x) (ps-path-y)))
    (move (ps-current-x)
	  (incf (ps-current-y) (ps-stack start)))
    (setf (ps-path-x) (ps-current-x)
	  (ps-path-y) (ps-current-y))
    (ps-clear-stack)))

(defun ps-rlineto ()
  ;; {dxa dya}+
  (lambda ()
    (loop for index below (length (ps-stack)) by 2
	  for dxa = (ps-stack index)
	  for dya = (ps-stack (1+ index))
	  do (line (incf (ps-current-x) dxa) (incf (ps-current-y) dya)))
    (ps-clear-stack)))

(defun ps-hlineto ()
  ;; dx1 {dya dxb}*
  ;; {dxa dyb}+
  (ps-[h/v]lineto t))

(defun ps-vlineto ()
  ;; dy1 {dxa dyb}*
  ;; {dya dxb}+
  (ps-[h/v]lineto nil))

(defun ps-rrcurveto ()
  ;;{dxa dya dxb dyb dxc dyc}+
  (ps-r[r]curve[line]to nil))

(defun ps-hhcurveto ()
  ;; dy1? {dxa dxb dyb dxc}+
  (lambda ()
    (let* ((count (length (ps-stack)))
	   (start (if (oddp count) 1 0)))
      (loop with (x1 y1 x2 y2 x3 y3)
	    for index from start below count by 4
	    do (setf x1 (+ (ps-stack (+ index 0)) (ps-current-x))
		     y1 (if (= start 1)
			    (prog1 (+ (ps-stack 0) (ps-current-y))
			      (setf start 0)) ;; FIXME - terrible pun
			    (ps-current-y))
		     x2 (+ (ps-stack (+ index 1)) x1)
		     y2 (+ (ps-stack (+ index 2)) y1)
		     x3 (+ (ps-stack (+ index 3)) x2)
		     y3 y2)
	       (curve x1 y1 x2 y2 x3 y3)
	       (setf (ps-current-x) x3
		     (ps-current-y) y3))
      (ps-clear-stack))))

(defun ps-hvcurveto ()
  ;; dx1 dx2 dy2 dy3 {dya dxb dyb dxc dxd dxe dye dyf}* dxf?
  ;; {dxa dxb dyb dyc dyd dxe dye dxf}+ dyf?
  (ps-[hv/vh]curveto t))

;; count = 5 (4)
;; 
;; + curX ix0    curY
;; + x1 ix1      + y1 ix2
;; + x2 ix4?     + y2 ix3
;;
;; count = 13 (12)
;;
;; curve 1
;; + curX ix0    curY
;; + x1 ix1      + y1 ix2
;; + x2          + y2 ix3
;;
;; curve 2
;; curX          + curY ix0
;; + x1 ix1      + y1 ix2
;; + x2 ix3      y2
;;
;; curve 3
;; + curX ix4    curY
;; + x1 ix5      + y1 ix6
;; + x1 ix8?     + y1 ix7
;;
;; count = 9 (8)
;;
;; curve1
;; + curX ix0    curY
;; + x1 ix1      + y1 ix2
;; + x2          + y2 ix3
;;
;; curve2
;; + x3          + y3 ix4
;; + x4 ix5      + y4 ix6
;; + x5 ix7     + y5 ix8?


;; x  y  - starting point                 curX          curY
;; x1 y1 - first control point            + curX ix0    curY
;; x2 y2 - second control point           + x1 ix1      + y1 ix2
;; x3 y3 - joining point                  + x2 ix4?     + y2 ix3
;; x4 y4 - third control point
;; x5 y5 - fourth and last control point
;; x6 y6 - ending point


(defun ps-rcurveline ()
  ;; {dxa dya dxb dyb dxc dyc}+ dxd dyd
  (ps-r[r]curve[line]to t))

(defun ps-rlinecurve ()
  ;; {dxa dya}+ dxb dyb dxc dyc dxd dyd
  (lambda ()
    (let ((index 0)
	  (count (length (ps-stack))))
      (loop while (< (+ index 6) count)
	    do (incf (ps-current-x) (ps-stack index))
	       (incf (ps-current-y) (ps-stack (1+ index)))
	       (line (ps-current-x) (ps-current-y))
	       (incf index 2))
      (loop with (x1 y1 x2 y2 x3 y3)
	    while (< index count)
	    do (setf x1 (+ (ps-stack (+ index 0)) (ps-current-x))
		     y1 (+ (ps-stack (+ index 1)) (ps-current-y))
		     x2 (+ (ps-stack (+ index 2)) x1)
		     y2 (+ (ps-stack (+ index 3)) y1)
		     x3 (+ (ps-stack (+ index 4)) x2)
		     y3 (+ (ps-stack (+ index 5)) y2))
	       (curve x1 y1 x2 y2 x3 y3)
	       (setf (ps-current-x) x3
		     (ps-current-y) y3)
	       (incf index 6))
      (ps-clear-stack))))

(defun ps-vhcurveto ()
  ;; dy1 dx2 dy2 dx3 {dxa dxb dyb dyc dyd dxe dye dxf}* dyf?
  ;; {dya dxb dyb dxc dxd dxe dye dyf}+ dxf?
  (ps-[hv/vh]curveto nil)
  #+(or)(let* ((count (length (ps-stack)))
	 (limit (if (oddp count) (1- count) count))
	 (index 0))
    (loop with (x1 y1 x2 y2 x3 y3)
	  while (< index limit)
	  do (setf x1 (ps-current-x)
		   y1 (+ (ps-stack (+ index 0)) (ps-current-y))
		   x2 (+ (ps-stack (+ index 1)) x1)
		   y2 (+ (ps-stack (+ index 2)) y1)
		   x3 (+ (ps-stack (+ index 3)) x2)
		   y3 (+ (if (= 5 (- index count))
			     (prog1 (ps-stack (+ index 4))
			       (incf index))
			     0)
			 y2))
	     (curve x1 y1 x2 y2 x3 y3)
	     (setf (ps-current-x) x3
		   (ps-current-y) y3)
	     (incf index 4)
	     (unless (< index limit)
	       (loop-finish))
	     (setf x1 (+ (ps-stack (+ index 0)) (ps-current-x))
		   y1 (ps-current-y)
		   x2 (+ (ps-stack (+ index 1)) x1)
		   y2 (+ (ps-stack (+ index 2)) y1)
		   y3 (+ (ps-stack (+ index 3)) y2)
		   x3 (+ (if (= 5 (- index count))
			     (prog1 (ps-stack (+ index 4))
			       (incf index))
			     0)
			 x2))
	     (curve x1 y1 x2 y2 x3 y3)
	     (setf (ps-current-x) x3
		   (ps-current-y) y3)
	     (incf index 4))))

	  

(defun ps-vvcurveto ()
  ;; dx1? {dya dxb dyb dyc}+
  (lambda ()
    (let* ((count (length (ps-stack)))
	   (start (if (oddp count) 1 0)))
      (loop with (x1 y1 x2 y2 x3 y3)
	    for index from start below count by 4
	    do (setf x1 (if (= start 1)
			    (prog1 (+ (ps-stack 0) (ps-current-x))
			      (setf start 0)) ;; FIXME - terrible pun
			    (ps-current-x))
		     y1 (+ (ps-stack (+ index 0)) (ps-current-y))
		     x2 (+ (ps-stack (+ index 1)) x1)
		     y2 (+ (ps-stack (+ index 2)) y1)
		     x3 x2
		     y3 (+ (ps-stack (+ index 3)) y2))
	       (curve x1 y1 x2 y2 x3 y3)
	       (setf (ps-current-x) x3
		     (ps-current-y) y3))
      (ps-clear-stack))))

(defun ps-flex ()
  ;; dx1 dy1 dx2 dy2 dx3 dy3 dx4 dy4 dx5 dy5 dx6 dy6 fd
  ;; ignoring the flex depth 'fd'
  (lambda ()
    (loop with (x1 y1 x2 y2 x3 y3)
	  repeat 2
	  for index from 0 by 6
	  do (setf x1 (+ (ps-stack (+ index 0)) (ps-current-x))
		   y1 (+ (ps-stack (+ index 1)) (ps-current-y))
		   x2 (+ (ps-stack (+ index 2)) x1)
		   y2 (+ (ps-stack (+ index 3)) y1)
		   x3 (+ (ps-stack (+ index 4)) x2)
		   y3 (+ (ps-stack (+ index 5)) y2))
	     (curve x1 y1 x2 y2 x3 y3)
	     (setf (ps-current-x) x3
		   (ps-current-y) y3))
    (ps-clear-stack)))

;; x  y  - starting point
;; x1 y1 - first control point
;; x2 y2 - second control point
;; x3 y3 - joining point
;; x4 y4 - third control point
;; x5 y5 - fourth and last control point
;; x6 y6 - ending point

(defun ps-hflex ()
  ;; dx1 dx2 dy2 dx3 dx4 dx5 dx6
  (lambda ()
    (let* ((x1 (+ (ps-stack 0) (ps-current-x)))
	   (y1 (ps-current-y))
	   (x2 (+ (ps-stack 1) x1))
	   (y2 (+ (ps-stack 2) y1))
	   (x3 (+ (ps-stack 3) x2))
	   (y3 y2)
	   (x4 (+ (ps-stack 4) x3))
	   (y4 y2)
	   (x5 (+ (ps-stack 5) x4))
	   (y5 y1)
	   (x6 (+ (ps-stack 6) x5))
	   (y6 y1))
      (curve x1 y1 x2 y2 x3 y3)
      (curve x4 y4 x5 y5 x6 y6)
      (setf (ps-current-x) x6)
      (ps-clear-stack))))

(defun ps-hflex1 ()
  ;; dx1 dy1 dx2 dy2 dx3 dx4 dx5 dy5 dx6
  (lambda ()
    (let* ((x1 (+ (ps-stack 0) (ps-current-x)))
	   (y1 (+ (ps-stack 1) (ps-current-y)))
	   (x2 (+ (ps-stack 2) x1))
	   (y2 (+ (ps-stack 3) y1))
	   (x3 (+ (ps-stack 4) x2))
	   (y3 y2)
	   (x4 (+ (ps-stack 5) x3))
	   (y4 y2)
	   (x5 (+ (ps-stack 6) x4))
	   (y5 (+ (ps-stack 7) y4))
	   (x6 (+ (ps-stack 8) x5))
	   (y6 (ps-current-y)))
      (curve x1 y1 x2 y2 x3 y3)
      (curve x4 y4 x5 y5 x6 y6)
      (setf (ps-current-x) x6)
      (ps-clear-stack))))

(defun ps-flex1 ()
  ;; dx1 dy1 dx2 dy2 dx3 dy3 dx4 dy4 dx5 dy5 d6
  (lambda ()
    (let* ((x1 (+ (ps-stack 0) (ps-current-x)))
	   (y1 (+ (ps-stack 1) (ps-current-y)))
	   (x2 (+ (ps-stack 2) x1))
	   (y2 (+ (ps-stack 3) y1))
	   (x3 (+ (ps-stack 4) x2))
	   (y3 (+ (ps-stack 5) y2))
	   (x4 (+ (ps-stack 6) x3))
	   (y4 (+ (ps-stack 7) y3))
	   (x5 (+ (ps-stack 8) x4))
	   (y5 (+ (ps-stack 9) y4))
	   (dx (- x5 (ps-current-x)))
	   (dy (- y5 (ps-current-y)))
	   (x6 (if (> (abs dx) (abs dy)) (+ (ps-stack 10) x5) (ps-current-x)))
	   (y6 (if (> (abs dx) (abs dy)) (ps-current-y) (+ (ps-stack 10) (ps-current-y)))))
      (curve x1 y1 x2 y2 x3 y3)
      (curve x4 y4 x5 y5 x6 y6)
      (setf (ps-current-x) x6
	    (ps-current-y) y6)
      (ps-clear-stack))))

(defun ps-[h/v]lineto (&optional horizontalp)
  (lambda ()
    (loop for value across (ps-stack)
	  for hp = horizontalp then (not hp)
	  if hp
	    do (incf (ps-current-x) value)
	  else
	    do (incf (ps-current-y) value)
	  do (line (ps-current-x) (ps-current-y)))
    (ps-clear-stack)))

(defun ps-r[r]curve[line]to (&optional linep)
  (lambda ()
    (let ((count (length (ps-stack)))
	  (index 0))
      (loop with (x1 y1 x2 y2 x3 y3)
	    while (<= (+ index 6) count)
	    do (setf x1 (+ (ps-stack (+ index 0)) (ps-current-x))
		     y1 (+ (ps-stack (+ index 1)) (ps-current-y))
		     x2 (+ (ps-stack (+ index 2)) x1)
		     y2 (+ (ps-stack (+ index 3)) y1)
		     x3 (+ (ps-stack (+ index 4)) x2)
		     y3 (+ (ps-stack (+ index 5)) y2))
	       (curve x1 y1 x2 y2 x3 y3)
	       (setf (ps-current-x) x3
		     (ps-current-y) y3)
	       (incf index 6))
      (when linep
	(incf (ps-current-x) (ps-stack index))
	(incf (ps-current-y) (ps-stack (1+ index)))
	(line (ps-current-x) (ps-current-y)))
      (ps-clear-stack))))

(defun ps-[hv/vh]curveto (&optional horizontal)
  (lambda ()
    (let ((count (length (ps-stack)))
	  (index 0))
      ;; FreeType adjusts the count and index in case it's not in the right form thus
      ;; (setf count (logand count (lognot 2))) ; i.e. a multiple of 4, or multiple of 4 + 1
      ;; (incf index (- (length (ps-stack)) count)) ; i.e. forwarding 2 if necessary
      (loop with (x1 y1 x2 y2 x3 y3)
	    for hp = horizontal then (not hp)
	    while (< index count)
	    if hp
	      do (setf x1 (+ (ps-stack (+ index 0)) (ps-current-x))
		       y1 (ps-current-y)
		       x2 (+ (ps-stack (+ index 1)) x1)
		       y2 (+ (ps-stack (+ index 2)) y1)
		       ;; need to do this first in case we increment index for x
		       y3 (+ (ps-stack (+ index 3)) y2)
		       x3 (if (= 5 (- count index))
			      (prog1
				  (+ (ps-stack (+ index 4)) x2)
				(incf index))
			      x2))
	    else
	      do (setf x1 (ps-current-x)
		       y1 (+ (ps-stack (+ index 0)) (ps-current-y))
		       x2 (+ (ps-stack (+ index 1)) x1)
		       y2 (+ (ps-stack (+ index 2)) y1)
		       x3 (+ (ps-stack (+ index 3)) x2)
		       y3 (if (= 5 (- count index))
			      (prog1
				  (+ (ps-stack (+ index 4)) y2)
				(incf index))
			      y2))
	    do (curve x1 y1 x2 y2 x3 y3)
	       (setf (ps-current-x) x3
		     (ps-current-y) y3)
	       (incf index 4))
      (ps-clear-stack))))

;;;## 4.2 Operator for Finishing a Path

(defun ps-endchar ()
  (lambda ()
    (unless (and (= (ps-current-x) (ps-path-x))
		 (= (ps-current-y) (ps-path-y)))
      (line (ps-path-x) (ps-path-y)))))

;;;## 4.3 Hint Operators

(defun ps-hstem ()
  (lambda () ;; FIXME need to recalculate at run time if 'random' used
    (ps-clear-stack)))

(defun ps-vstem ()
  (lambda ()
    (ps-clear-stack)))

(defun ps-hstemhm ()
  (lambda ()
    (ps-clear-stack)))

(defun ps-vstemhm ()
  (lambda ()
    (ps-clear-stack)))

(defun ps-hintmask (index)
  (lambda ()
    (setf (ps-hintmask-index) index)
    (ps-clear-stack)))

(defun ps-cntrmask ()
  (lambda ()
    (ps-clear-stack)))

;;;## 4.4 Arithmetic Operators

(defun ps-abs ()
  (lambda () (ps-push (abs (ps-pop)))))

(defun ps-add ()
  (lambda () (ps-push (+ (ps-pop) (ps-pop)))))

(defun ps-sub ()
  (lambda () (ps-push (- (ps-pop) (ps-pop)))))

(defun ps-div ()
  (lambda () (ps-push (/ (ps-pop) (ps-pop))))) ;; FIXME 0 on underflow

(defun ps-neg ()
  (lambda () (ps-push (- (ps-pop)))))

(defun ps-random ()
  (lambda () (ps-push (random 1.0))))

(defun ps-mul ()
  (lambda () (ps-push (* (ps-pop) (ps-pop))))) ;; FIXME 0 on underflow

(defun ps-sqrt ()
  (lambda () (ps-push (sqrt (ps-pop)))))

(defun ps-drop ()
  (lambda () (ps-pop)))

(defun ps-exch ()
  (lambda ()
    (let ((length (length (ps-stack))))
      (rotatef (aref (ps-stack) (1- length))
	       (aref (ps-stack) (- length 2))))))

(defun ps-index ()
  (lambda ()
    (let ((index (ps-pop)))
      (if (minusp index)
	  (ps-push (aref (ps-stack) 0))
	  (let ((length (length (ps-stack))))
	    (ps-push (aref (ps-stack) (1- (- length index)))))))))

(defun ps-roll () ;; FIXME - wrong direction?
  (lambda ()
    (let ((j (ps-pop))
	  (n (ps-pop))
	  (length (length (ps-stack))))
      (cond ((< n 2)
	     nil) ;; nothing to do
	    ((> n length)
	     nil) ;; or error
	    (t
	     (setf j (mod j n))
	     (if (zerop j)
		 nil ;; nothing to do
		 (let ((sub (make-array n :displaced-to (ps-stack) :displaced-index-offset (- length n)))
		       (start-index -1)
		       (index -1))
		   (loop with temp = nil
			 with last = nil
			 for i below n
			 when (= start-index index)
			   do (incf start-index)
			      (setf index start-index
				    last (aref sub index))
			 do (incf index j)
			    (cond ((>= index n)
				   (decf index n))
				  ((minusp index)
				   (incf index n)))
			    (setf temp (aref sub index) ;; FIXME rotatef
				  (aref sub index) last
				  last temp)))))))))

(defun ps-dup ()
  (lambda ()
    (ps-push (aref (ps-stack) (1- (length (ps-stack)))))))

;;;## 4.5 Storage Operators

(defun ps-put ()
  (lambda ()
    (let ((i (ps-pop))
	  (val (ps-pop)))
      (setf (aref (ps-transient-array) i) val))))

(defun ps-get ()
  (lambda ()
    (let ((i (ps-pop)))
      (aref (ps-transient-array) i))))

;;## 4.6 Conditional Operators

(defun ps-and ()
  (lambda ()
    (let ((num2 (ps-pop))
	  (num1 (ps-pop)))
      (if (or (zerop num1)
	      (zerop num2))
	  (ps-push 0)
	  (ps-push 1)))))

(defun ps-or ()
  (lambda ()
    (let ((num2 (ps-pop))
	  (num1 (ps-pop)))
      (if (and (zerop num1)
	       (zerop num2))
	  (ps-push 0)
	  (ps-push 1)))))

(defun ps-not ()
  (lambda ()
    (let ((num1 (ps-pop)))
      (if (zerop num1) (ps-push 1) (ps-push 0)))))

(defun ps-eq ()
  (lambda ()
    (if (= (ps-pop) (ps-pop)) (ps-push 1) (ps-push 0))))

(defun ps-ifelse ()
  (lambda ()
    (let ((v2 (ps-pop))
	  (v1 (ps-pop))
	  (s2 (ps-pop))
	  (s1 (ps-pop)))
      (if (<= v1 v2) (ps-push s1) (ps-push s2)))))

;;;## 4.7 Subroutine Operations

(defun ps-callsubr ()
  (lambda ()
    (funcall (ps-subroutine (ps-pop)))))

(defun ps-callgsubr ()
  (lambda ()
    (funcall (ps-global-subroutine (ps-pop)))))

;; return = NOP

(defun read-type2-charstring (stream &optional (nominal-width-x 0) (default-width-x 0))
  (loop with width = nil
	with randomp = nil
	with cntrmasks = '()
	with hintmasks = '()
	with hstems = '()
	with vstems = '()
	with code-vector = (make-array 0 :fill-pointer 0)
	with stack = (make-array 0 :fill-pointer 0)
	with end = nil
	for b0 = (read-byte stream nil :eof)
	until (eq :eof b0)
	until end
	do (let ((code
		   (case b0
		     ;; 0 -Reserved-
		     (1 ; hstem
		      (let ((length (length stack))
			    (start 0))
			;; check for width
			(if (oddp length)
			    (setf width (+ nominal-width-x (aref stack 0))
				  start 1)
			    (setf width t))
			(loop for index from (1- length) downto start
			      do (push (aref stack index) hstems))
			(setf (fill-pointer stack) 0))
		      (ps-hstem))
		     ;; 2 -Reserved-
		     (3 ; vstem
		      (let ((length (length stack))
			    (start 0))
			;; check for width
			(if (and (null width) (oddp length))
			    (setf width (+ nominal-width-x (aref stack 0))
				  start 1)
			    (setf width t))
			(loop for index from (1- length) downto start
			      do (push (aref stack index) vstems))
			(setf (fill-pointer stack) 0))
		      (ps-vstem))
		     (4 ; vmoveto
		      (let ((length (length stack))
			    (start 0))
			(if (and (null width) (> length 1))
			    (setf width (+ nominal-width-x (aref stack 0))
				  start 1)
			    (setf width t))
			(setf (fill-pointer stack) 0)
			(ps-vmoveto start)))
		     (5 ; rlineto
		      (setf (fill-pointer stack) 0)
		      (ps-rlineto))
		     (6 ; hlineto
		      (setf (fill-pointer stack) 0)
		      (ps-hlineto))
		     (7 ; vlineto
		      (setf (fill-pointer stack) 0)
		      (ps-vlineto))
		     (8 ; rrcurveto
		      (setf (fill-pointer stack) 0)
		      (ps-rrcurveto))
		     ;; 9 -Reserved-
		     (10 ; callsubr
		      (ps-callsubr))
		     (11 ; return
		      nil)
		     (12
		      (let ((b1 (read-byte stream)))
			(case b1
			  ;; 0 -Reserved-
			  ;; 1 -Reserved-
			  ;; 2 -Reserved-
			  (3 ; and
			   (ps-and))
			  (4 ; or
			   (ps-or))
			  (5 ; not
			   (ps-not))
			  ;; 6 -Reserved-
			  ;; 7 -Reserved-
			  ;; 8 -Reserved-
			  (9 ; abs
			   (ps-abs))
			  (10 ; add
			   (ps-add))
			  (11 ; sub
			   (ps-sub))
			  (12 ; div
			   (ps-div))
			  ;; 13 -Reserved-
			  (14 ; neg
			   (ps-neg))
			  (15 ; eq
			   (ps-eq))
			  ;; 16 -Reserved-
			  ;; 17 -Reserved-
			  (18 ;drop
			   (ps-drop))
			  ;; 19 -Reserved-
			  (20 ; put
			   (ps-put))
			  (21 ; get
			   (ps-get))
			  (22 ; ifelse
			   (ps-ifelse))
			  (23 ; random
			   (setf randomp t)
			   (ps-random))
			  (24 ; mul
			   (ps-mul))
			  ;; 25 -Reserved-
			  (26 ; sqrt
			   (ps-sqrt))
			  (27 ; dup
			   (ps-dup))
			  (28 ; exch
			   (ps-exch))
			  (29 ; index
			   (ps-index))
			  (30 ; roll
			   (ps-roll))
			  ;; 31 -Reserved-
			  ;; 32 -Reserved-
			  ;; 33 -Reserved-
			  (34 ; hflex
			   (setf (fill-pointer stack) 0)
			   (ps-hflex))
			  (35 ; flex
			   (setf (fill-pointer stack) 0)
			   (ps-flex))
			  (36 ; hflex1
			   (setf (fill-pointer stack) 0)
			   (ps-hflex1))
			  (37 ; flex1
			   (setf (fill-pointer stack) 0)
			   (ps-flex1)))))
		     ;; 13 -Reserved-
		     (14 ; endchar
		      (if (and (null width) (plusp (length stack)))
			  (setf width (aref stack 0))
			  (setf width t))
		      (setf end t)
		      (ps-endchar))
		     ;; 15 -Reserved-
		     ;; 16 -Reserved-
		     ;; 17 -Reserved-
		     (18 ; hstemhm
		      (let ((length (length stack))
			    (start 0))
			;; check for width
			(if (oddp length)
			    (setf width (+ nominal-width-x (aref stack 0))
				  start 1)
			    (setf width t))
			(loop for index from (1- length) downto start
			      do (push (aref stack index) hstems))
			(setf (fill-pointer stack) 0))
		      (ps-hstemhm))
		     (19 ; hintmask
		      (if (and (null width) (null vstems) (null hstems) (plusp (length stack)))
			  (setf width (aref stack 0))
			  (setf width t))
		      (when (and hstems (null vstems) (plusp (length stack)))
			(loop for index from (1- (length stack)) downto 0
			      do (push (aref stack index) vstems)))
		      (setf (fill-pointer stack) 0)
		      (let* ((hint-count (/ (+ (length hstems) (length vstems)) 2))
			     (byte-count (ash (+ 7 hint-count) -3)))
			(when (plusp hint-count)
			  (push (loop repeat byte-count collect (read-byte stream)) hintmasks)
			  (ps-hintmask (1- (length hintmasks))))))
		     (20 ; cntrmask
		      (if (and (null width) (plusp (length stack)))
			  (setf width (aref stack 0)
				(fill-pointer stack) 0)
			  (setf width t))
		      (let* ((hint-count (/ (+ (length hstems) (length vstems)) 2))
			     (byte-count (ash (+ hint-count 7) -3)))
			(when (plusp hint-count)
			  (push (loop repeat byte-count collect (read-byte stream)) cntrmasks)))
		      (setf (fill-pointer stack) 0)
		      (ps-cntrmask))
		     (21 ; rmoveto
		      (let ((length (length stack))
			    (start 0))
			(if (and (null width) (> length 2))
			    (setf width (+ nominal-width-x (aref stack 0))
				  start 1)
			    (setf width t))
			(setf (fill-pointer stack) 0)
			(ps-rmoveto start)))
		     (22 ; hmoveto
		      (let ((length (length stack))
			    (start 0))
			(if (and (null width) (> length 1))
			    (setf width (+ nominal-width-x (aref stack 0))
				  start 1)
			    (setf width t))
			(setf (fill-pointer stack) 0)
			(ps-hmoveto start)))
		     (23 ; vstemhm
		      (let ((length (length stack))
			    (start 0))
			;; check for width
			(if (and (null width) (oddp length))
			    (setf width (+ nominal-width-x (aref stack 0))
				  start 1)
			    (setf width t))
			(loop for index from (1- length) downto start
			      do (push (aref stack index) vstems))
			(setf (fill-pointer stack) 0))
		      (ps-vstemhm))
		     (24 ; rcurveline
		      (setf (fill-pointer stack) 0)
		      (ps-rcurveline))
		     (25 ; rlinecurve
		      (setf (fill-pointer stack) 0)
		      (ps-rlinecurve))
		     (26 ; vvcurveto
		      (setf (fill-pointer stack) 0)
		      (ps-vvcurveto))
		     (27 ; hhcurveto
		      (setf (fill-pointer stack) 0)
		      (ps-hhcurveto))
		     ;; 28 shortint
		     (29 ; callgsubr
		      (ps-callgsubr))
		     (30 ; vhcurveto
		      (setf (fill-pointer stack) 0)
		      (ps-vhcurveto))
		     (31 ; hvcurveto
		      (setf (fill-pointer stack) 0)
		      (ps-hvcurveto))
		     ;; 32-246 <numbers>
		     ;; 247-254 <numbers> - 2 byte sequence
		     ;; 255 <numbers> - 5 byte sequence
		     (otherwise
		      (let ((number (read-type2-operand stream b0)))
			(when number
			  (vector-push-extend number stack)
			  (lambda () (ps-push number)))))
		     )))
	     (if code
		 (vector-push-extend code code-vector)
		 (warn "Unknown charstring code ~D" b0)))
	finally (return (make-instance 'cff-glyph
				       :code code-vector
				       :width (if (numberp width) width default-width-x)
				       :hstems hstems
				       :vstems vstems
				       :hintmasks (nreverse hintmasks)
				       :cntrmasks (nreverse cntrmasks)
				       :randomp randomp))))

;;;# MOVE LINE CURVE CLOSE-PATH

(defvar *device* nil)

(defgeneric move-to (device x y))

(defun move (x y)
  (move-to *device* x y))

(defgeneric line-to (device x y))

(defun line (x y)
  (line-to *device* x y))

(defgeneric curve-to (device x1 y1 x2 y2 x3 y3))

(defun curve (x1 y1 x2 y2 x3 y3)
  (curve-to *device* x1 y1 x2 y2 x3 y3))

(defgeneric close-path (device))

(defun close-path* ()
  (close-path *device*))

;;;# SVG testing

(defclass svg-device ()
  ())

(defparameter *points* nil)
(defparameter *control-points* nil)

(defmethod move-to ((device svg-device) x y)
  (push (cons x (- 500 y)) *points*)
  (format t "M ~A ~A " x (- 500 y)))

(defmethod line-to ((device svg-device) x y)
  (push (cons x (- 500 y)) *points*)
  (format t "L ~A ~A " x (- 500 y)))

(defmethod curve-to ((device svg-device) x1 y1 x2 y2 x3 y3)
  (push (cons x3 (- 500 y3)) *points*)
  (push (cons x1 (- 500 y1)) *control-points*)
  (push (cons x2 (- 500 y2)) *control-points*)
  (format t "C ~A ~A ~A ~A ~A ~A " x1 (- 500 y1) x2 (- 500 y2) x3 (- 500 y3)))

(defmethod close-path ((device svg-device))
  (format t "Z"))

;;;# TESTING

(defparameter *device* nil)

(defmacro with-glyph ((glyph device) &body body)
  `(let ((*context* (make-instance 'cff-context :glyph ,glyph))
	 (*device* ,device))
     ,@body))

(defun make-glyph (font character-name)
  (let* ((sid (string-sid font character-name))
	 (name-index (or (position sid (cff-charsets font)) 0)) ;; DEBUG FIXME
	 (glyph (read-type2-charstring
		 (make-octet-vector-stream (character-index-glyph font (1+ name-index)))
		 (nominal-width-x font)
		 (default-width-x font))))
    (setf (glyph-name glyph) (octets-latin1 character-name))
    glyph))

(defparameter *glyph-test-directory* #P"c:/Users/David/Downloads/")

(defun do-glyph (glyph)
  (let ((*context* (make-instance 'cff-context :glyph glyph))
	(*device* (make-instance 'svg-device))
	(pathname (make-pathname :defaults *glyph-test-directory* :name (glyph-name glyph) :type "svg"))
	(*points* '())
	(*control-points* '()))
    (with-open-file (*standard-output* pathname :direction :output :if-exists :supersede)
      (format t "<?xml version=\"1.0\" standalone=\"no\"?>~%")
      (format t "<svg width=\"1500\" height=\"2000\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">~%")
      (format t "<g transform=\"translate(500,500)\" fill=\"none\" stroke=\"black\" >~%")
      (format t "~%<line x1='0' x2='1000' y1='500' y2='500' stroke='DarkGray' />")
      (format t "~%<line x1='0' x2='0' y1='500' y2='-500' stroke='DarkGray' />")
      (loop for hstem in (glyph-hstems glyph)
	    for accum = hstem then (+ accum hstem)
	    do (format t "~%<line x1='0' x2='1000' y1='~D' y2='~D' stroke='CornflowerBlue' />" (- 500 accum) (- 500 accum)))
      (loop for vstem in (glyph-vstems glyph)
	    for accum = vstem then (+ accum vstem)
	    do (format t "~%<line x1='~D' x2='~D' y1='0' y2='500' stroke='DarkSeaGreen' />" accum accum))
      (format t "~&<path d=\"")
      (loop for function across (glyph-code-vector glyph)
	    do (funcall function))
      (format t "\"/>~%")
      (loop for (x . y) in *points*
	    do (format t "~%<rect width='10' height='10' x='~D' y='~D' stroke='green'/>" (- x 5) (- y 5)))
      (loop for (x . y) in *control-points*
	    do (format t "~%<circle r='5' cx='~D' cy='~D' stroke='red'/>" x y))
      (format t "~%</g>~%</svg>"))
    pathname))

;;;# TYPE 1

(defclass type1-context (ps-context)
  ((%vstems
    :accessor type1-context-vstems
    :initform nil)
   (%hstems
    :accessor type1-context-hstems
    :initform nil)
   (%dotsection
    :accessor type1-context-dotsection
    :initform nil)
   (%postscript-stack
    :accessor type1-context-postscript-operand-stack
    :initform (make-array 0 :fill-pointer 0))))

(defmacro ps-current-vstems ()
  '(type1-context-vstems *context*))

(defmacro ps-current-hstems ()
  '(type1-context-hstems *context*))

(defmacro ps-current-dotsection ()
  '(type1-context-dotsection *context*))

(defmacro ps-postcript-stack (&optional index)
  (if index
      `(aref (type1-context-postscript-operand-stack *context*) ,index)
      '(type1-context-postscript-operand-stack *context*)))

(defun type1-othersubroutine (num)
  (type1-othersubroutine (ps-context-font *context*) num))

(defun type1-subroutine (num)
  (type1-subroutine (ps-context-font *context*) num))

(defun type1-endchar ()
  (lambda ()
    (ps-clear-stack)))

;; "This command aslo sets the current point to (sbx, 0) but does not place
;; the point in the character path."
(defun type1-hsbw ()
  (lambda ()
    (setf (ps-current-x) (ps-stack 0))
    (ps-clear-stack)))

(defun type1-seac ()
  (let ((asb (ps-stack 0))
	;; offsets from left-sidebearing point of base char to lsb of accent???
	;; see Errata in Type 1 Supplement
	(adx (ps-stack 1))
	(ady (ps-stack 2))
	(bx (ps-current-x))
	(by (ps-current-y))
	(bchar (ps-stack 3))
	(achar (ps-stack 4)))
    (ps-clear-stack)
    (type1-call-charstring bchar)
    (setf (ps-current-x) (+ adx bx)
	  (ps-current-y) (+ ady by)) ;; FIXME ???
    (type1-call-charstring achar)
    (ps-clear-stack)))

(defun type1-sbw ()
  (lambda ()
    (setf (ps-current-x) (ps-stack 0)
	  (ps-current-y) (ps-stack 1))
    (ps-clear-stack)))

(defun type1-closepath ()
  (lambda ()
    (unless (and (= (ps-current-x) (ps-path-x))
		 (= (ps-current-y) (ps-path-y)))
      (line (ps-path-x) (ps-path-y)))))

(defun type1-hlineto ()
  (lambda ()
    (let ((dx (ps-stack 0)))
      (line (incf (ps-current-x) dx) (ps-current-y))
      (ps-clear-stack))))

(defun type1-hmoveto ()
  (lambda ()
    (let ((dx (ps-stack 0)))
      (move (incf (ps-current-x) dx) (ps-current-y))
      (setf (ps-path-x) (ps-current-x)
	    (ps-path-y) (ps-current-y))
      (ps-clear-stack))))

(defun type1-hvcurveto ()
  (lambda ()
    (let* ((dx1 (ps-stack 0))
	   (dx2 (ps-stack 1))
	   (dy2 (ps-stack 2))
	   (dy3 (ps-stack 3))
	   (x1 (+ dx1 (ps-current-x)))
	   (y1 (ps-current-y))
	   (x2 (+ dx2 x1))
	   (y2 (+ dy2 y1))
	   (x3 x2)
	   (y3 (+ dy3 y2)))
      (curve x1 y1 x2 y2 x3 y3)
      (setf (ps-current-x) x3
	    (ps-current-y) y3)
      (ps-clear-stack))))

(defun type1-rlineto ()
  (lambda ()
    (line (incf (ps-current-x) (ps-stack 0))
	  (incf (ps-current-y) (ps-stack 1)))
    (ps-clear-stack)))

(defun type1-rmoveto ()
  (lambda ()
    (move (incf (ps-current-x) (ps-stack 0))
	  (incf (ps-current-y) (ps-stack 1)))
    (setf (ps-path-x) (ps-current-x)
	  (ps-path-y) (ps-current-y))
    (ps-clear-stack)))

(defun type1-rrcurveto ()
  (lambda ()
    (let* ((dx1 (ps-stack 0))
	   (dy1 (ps-stack 1))
	   (dx2 (ps-stack 2))
	   (dy2 (ps-stack 3))
	   (dx3 (ps-stack 4))
	   (dy3 (ps-stack 5))
	   (x1 (+ dx1 (ps-current-x)))
	   (y1 (+ dy1 (ps-current-y)))
	   (x2 (+ dx2 x1))
	   (y2 (+ dy2 y1))
	   (x3 (+ dx3 x2))
	   (y3 (+ dy3 y2)))
      (curve x1 y1 x2 y2 x3 y3)
      (setf (ps-current-x) x3
	    (ps-current-y) y3)
      (ps-clear-stack))))

(defun type1-vhcurveto ()
  (lambda ()
    (let* ((dy1 (ps-stack 0))
	   (dx2 (ps-stack 1))
	   (dy2 (ps-stack 2))
	   (dx3 (ps-stack 3))
	   (x1 (ps-current-x))
	   (y1 (+ dy1 (ps-current-y)))
	   (x2 (+ dx2 x1))
	   (y2 (+ dy2 y1))
	   (x3 (+ dx3 x2))
	   (y3 y2))
      (curve x1 y1 x2 y2 x3 y3)
      (setf (ps-current-x) x3
	    (ps-current-y) y3)
      (ps-clear-stack))))

(defun type1-vlineto ()
  (lambda ()
    (line (ps-current-x) (incf (ps-current-y) (ps-stack 0)))
    (ps-clear-stack)))

(defun type1-vmoveto ()
  (lambda ()
    (move (ps-current-x) (incf (ps-current-y) (ps-stack 0)))
    (setf (ps-path-x) (ps-current-x)
	  (ps-path-y) (ps-current-y))
    (ps-clear-stack)))

(defun type1-dotsection ()
  (lambda ()
    (setf (ps-current-dotsection)
	  (not (ps-current-dotsection)))
    (ps-clear-stack)))

(defun type1-hstem ()
  (lambda ()
    (let ((y (ps-stack 0))
	  (dy (ps-stack 1)))
      (push (cons y (+ y dy)) (ps-current-hstems))
      (ps-clear-stack))))

(defun type1-hstem3 ()
  (lambda ()
    (let ((y0 (ps-stack 0))
	  (dy0 (ps-stack 1))
	  (y1 (ps-stack 2))
	  (dy1 (ps-stack 3))
	  (y2 (ps-stack 4))
	  (dy2 (ps-stack 5)))
      (push (cons y0 (+ y0 dy0)) (ps-current-hstems))
      (push (cons y1 (+ y1 dy1)) (ps-current-hstems))
      (push (cons y2 (+ y2 dy2)) (ps-current-hstems))
      (ps-clear-stack))))

(defun type1-vstem ()
  (lambda ()
    (let ((x (ps-stack 0))
	  (dx (ps-stack 1)))
      (push (cons x (+ x dx)) (ps-current-vstems))
      (ps-clear-stack))))

(defun type1-vstem3 ()
  (lambda ()
    (let ((x0 (ps-stack 0))
	  (dx0 (ps-stack 1))
	  (x1 (ps-stack 2))
	  (dx1 (ps-stack 3))
	  (x2 (ps-stack 4))
	  (dx2 (ps-stack 5)))
      (push (cons x0 (+ x0 dx0)) (ps-current-vstems))
      (push (cons x1 (+ x1 dx1)) (ps-current-vstems))
      (push (cons x2 (+ x2 dx2)) (ps-current-vstems))
      (ps-clear-stack))))

(defun type1-div ()
  (lambda ()
    (let ((num2 (ps-pop))
	  (num1 (ps-pop)))
      (ps-push (/ num2 num1)))))

(defun type1-callothersubr ()
  (lambda ()
    (let ((othersubr (ps-pop))
	  (argument-count (ps-pop)))
      (loop repeat argument-count
	    do (vector-push-extend (ps-pop) (ps-postscript-stack)))
      (type1-othersubroutine othersubr))))

(defun type1-callsubr ()
  (lambda ()
    (let ((subroutine (ps-pop)))
      (type1-subroutine subroutine))))

(defun type1-pop ()
  (lambda ()
    (ps-push (vector-pop (ps-postscript-stack))))) 

(defun type1-return ()
  (lambda ()
    (values)))

(defun type1-setcurrentpoint ()
  (lambda ()
    (let ((x (ps-stack 0))
	  (y (ps-stack 1)))
      (setf (ps-current-x) x
	    (ps-current-y) y)
      (ps-clear-stack))))

(defun read-type1-operand (stream b0)
  (cond
    ((<= 32 b0 246)
     (- b0 139))
    ((<= 247 b0 250)
     (let ((b1 (read-byte stream)))
       (+ (* 256 (- b0 247)) b1 108)))
    ((<= 251 b0 254)
     (let ((b1 (read-byte stream)))
       (- (* -256 (- b0 251)) b1 108)))
    ((= 255 b0)
     (read-int32 stream))))

(defun read-type1-charstring (stream)
  (loop with stack = (make-array 0 :fill-pointer 0)
	with code-vector = (make-array 0 :fill-pointer 0)
	with left-sidebearing = nil
	with width = nil
	with bounding-box = (vector 0 0 0 0)
	with end = nil
	for b0 = (read-byte stream nil :eof)
	until (eq :eof b0)
	until end
	do (let ((code
		   (case b0
		     ;; 0 -Reserved-
		     (1 ;; hstem
		      (setf (fill-pointer stack) 0)
		      (type1-hstem))
		     ;; 2 -Reserved-
		     (3 ;; vstem
		      (setf (fill-pointer stack) 0)
		      (type1-vstem))
		     (4 ;; vmoveto
		      (setf (fill-pointer stack) 0)
		      (type1-vmoveto))
		     (5 ;; rlineto
		      (setf (fill-pointer stack) 0)
		      (type1-rlineto))
		     (6 ;; hlineto
		      (setf (fill-pointer stack) 0)
		      (type1-hlineto))
		     (7 ;; vlineto
		      (setf (fill-pointer stack) 0)
		      (type1-vlineto))
		     (8 ;; rrcurveto
		      (setf (fill-pointer stack) 0)
		      (type1-rrcurveto))
		     (9 ;; closepath
		      (setf (fill-pointer stack) 0)
		      (type1-closepath))
		     (10 ;; callsubr
		      (setf (fill-pointer stack) 0)
		      (type1-callsubr))
		     (11 ;; return
		      (type1-return)) ;; FIXME Nothing to do?
		     (12
		      (let ((b1 (read-byte stream)))
			(case b1
			  (0 ;; dotsection
			   (setf (fill-pointer stack) 0)
			   (type1-dotsection))
			  (1 ;; vstem3
			   (setf (fill-pointer stack) 0)
			   (type1-vstem3))
			  (2 ;; hstem3
			   (setf (fill-pointer stack) 0)
			   (type1-hstem3))
			  (6 ;; seac
			   (setf (fill-pointer-stack) 0)
			   (type1-seac))
			  (7 ;; sbw
			   (let ((sbx (aref stack 0))
				 (sby (aref stack 1))
				 (wx (aref stack 2))
				 (wy (aref stack 3)))
			     (setf left-sidebearing (vector sbx sby)
				   width (vector wx wy)
				   (fill-pointer stack) 0)
			     (type1-sbw)))
			  (12 ;; div
			   (type1-div))
			  (16 ;; callothersubr
			   (type1-callothersubr))
			  (17 ;; pop
			   (type1-pop))
			  (33 ;; setcurrentpoint
			   (setf (fill-pointer stack) 0)
			   (type1-setcurrentpoint)))))
		     (13 ;; hsbw
		      (setf left-sidebearing-point (vector (aref stack 0) 0)
			    width (vector (aref stack 1) 0)
			    (fill-pointer stack) 0)
		      (type1-hsbw))
		     (14 ;; endchar
		      (setf end t)
		      (type1-endchar))
		     (21 ;; rmoveto
		      (setf (fill-pointer stack) 0)
		      (type1-rmoveto))
		     (22 ;; hmoveto
		      (setf (fill-pointer stack) 0)
		      (type1-hmoveto))
		     (30 ;; vhcurveto
		      (setf (fill-pointer stack) 0)
		      (type1-vhcurveto))
		     (31 ;; hvcurveto
		      (setf (fill-pointer stack) 0)
		      (type1-hvcurveto))
		     (otherwise
		      (let ((number (read-type1-operand stream b0)))
			(when number
			  (vector-push-extend number stack)
			  (lambda () (ps-push number)))))
		     )))
	     	     (if code
			 (vector-push-extend code code-vector)
			 (warn "Unknown charstring code ~D" b0)))
	finally (return (make-instance 'type1-glyph
				       :code code-vector
				       :width width
				       :left-sidebearing left-sidebearing))))

;;;# TEST

(defun do-type1-glyph (glyph)
  (let ((*context* (make-instance 'type1-context :glyph glyph))
	(*device* (make-instance 'svg-device))
	(pathname (make-pathname :defaults *glyph-test-directory* :name "glyph" :type "svg"))
	(*points* '())
	(*control-points* '()))
    (with-open-file (*standard-output* pathname :direction :output :if-exists :supersede)
      (format t "<?xml version=\"1.0\" standalone=\"no\"?>~%")
      (format t "<svg width=\"1500\" height=\"2000\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">~%")
      (format t "<g transform=\"translate(500,500)\" fill=\"none\" stroke=\"black\" >~%")
      (format t "~%<line x1='0' x2='1000' y1='500' y2='500' stroke='DarkGray' />")
      (format t "~%<line x1='0' x2='0' y1='500' y2='-500' stroke='DarkGray' />")

      (format t "~&<path d=\"")
      (loop for function across (glyph-code-vector glyph)
	    do (funcall function))
      (format t "\"/>~%")
      (loop for (x . y) in *points*
	    do (format t "~%<rect width='10' height='10' x='~D' y='~D' stroke='green'/>" (- x 5) (- y 5)))
      (loop for (x . y) in *control-points*
	    do (format t "~%<circle r='5' cx='~D' cy='~D' stroke='red'/>" x y))
      (format t "~%</g>~%</svg>"))
    pathname))
