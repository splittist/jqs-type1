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
   (%transient-array
    :initform (make-array 32)
    :accessor ps-context-transient-array)
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
   (%hintmask-index
    :initform 0
    :accessor ps-context-hintmask-index)))

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
  '(ps-context-transient-array *context*))

(defmacro ps-hintmask-index ()
  '(ps-context-hintmask-index *context*))

(defun ps-push (element)
  (vector-push element (ps-stack)))

(defun ps-pop ()
  (vector-pop (ps-stack)))

(defun ps-clear-stack ()
  (setf (fill-pointer (ps-stack)) 0))

(defun ps-subroutine (num)
  (cff-subroutine (ps-context-font *context*) num))

(defun ps-global-subroutine (num)
  (cff-global-subroutine (ps-context-font *context* num)))

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
	for b0 = (read-byte stream nil :eof)
	until (eq :eof b0)
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
		      (ps-endchar)
		      (loop-finish))
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
	finally (return (make-instance 'glyph
				       :code code-vector
				       :width (if (numberp width) width default-width-x)
				       :hstems hstems
				       :vstems vstems
				       :hintmasks (nreverse hintmasks)
				       :cntrmasks (nreverse cntrmasks)
				       :randomp randomp))))

;;;# MOVE LINE CURVE CLOSE-PATH

(defparameter *points* nil)
(defparameter *control-points* nil)

(defun move (x y)
  (push (cons x (- 500 y)) *points*)
  (format t "M ~A ~A " x (- 500 y)))

(defun line (x y)
  (push (cons x (- 500 y)) *points*)
  (format t "L ~A ~A " x (- 500 y)))

(defun curve (x1 y1 x2 y2 x3 y3)
  (push (cons x3 (- 500 y3)) *points*)
  (push (cons x1 (- 500 y1)) *control-points*)
  (push (cons x2 (- 500 y2)) *control-points*)
  (format t "C ~A ~A ~A ~A ~A ~A " x1 (- 500 y1) x2 (- 500 y2) x3 (- 500 y3)))

(defun close-path ()
  (format t "Z"))

;;;# TESTING

(defun make-glyph (font character-code)
  (serapeum:lret ((glyph (read-type2-charstring
			  (make-octet-vector-stream (character-code-glyph font character-code))
			  (nominal-width-x font)
			  (default-width-x font))))
    (setf (glyph-name glyph) (octets-latin1 (character-code-name font character-code)))))

(defparameter *glyph-test-directory* #P"c:/Users/David/Downloads/")

(defun do-glyph (glyph)
  (let ((*context* (make-instance 'ps-context :glyph glyph))
	(pathname (make-pathname :defaults *glyph-test-directory* :name (glyph-name glyph) :type "svg"))
	(*points* '())
	(*control-points* '()))
    (with-open-file (*standard-output* pathname :direction :output :if-exists :supersede)
      (format t "<?xml version=\"1.0\" standalone=\"no\"?>~%")
      (format t "<svg width=\"1500\" height=\"2000\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">~%")
      (format t "<g transform=\"translate(500,500)\" fill=\"none\" stroke=\"black\" >~%")
      (format t "~%<line x1='0' x2='1000' y1='500' y2='500' stroke='DarkGray' />")
      (format t "~%<line x1='0' x2='0' y1='500' y2='-500' storke='DarkGray' />")
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

