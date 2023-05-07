;;;; jqs-type1

(in-package #:com.splittist.type1)

(named-readtables:in-readtable syntax)

;;;# OPERATOR NAMES

(defparameter +operators+
  #(
    ;; Operand Stack Maninpulation Operators
    #"pop"
    #"exch"
    #"dup"
    #"copy"
    #"index"
    #"roll"
    #"clear"
    #"count"
    #"mark"
    #"cleartomark"
    #"countomark"
    
    ;; Arithmetic and Math Operators
    #"add"
    #"div"
    #"idiv"
    #"mod"
    #"mul"
    #"sub"
    #"abs"
    #"neg"
    #"ceiling"
    #"floor"
    #"round"
    #"truncate"
    #"sqrt"
    #"atan"
    #"cos"
    #"sin"
    #"exp"
    #"ln"
    #"log"
    #"rand"
    #"srand"
    #"rrand"
    #"array"
    #"["
    #"]"
    #"length"
    #"get"
    #"put"
    #"getinterval"
    #"putinterval"
    #"astore"
    #"aload"
    #"copy"
    #"forall"
    
    ;; Packed Array Operators
    #"packedarray"
    #"currentpacking"
    #"setpacking"
    ;; length, get, getinterval, aload, copy, forall
    
    ;; Dictionary Operators
    #"dict"
    #"<<"
    #">>"
    ;; length
    #"maxlength"
    #"begin"
    #"end"
    #"def"
    #"load"
    #"store"
    ;; get, put
    #"known"
    #"where"
    ;; copy, forall
    #"currentdict"
    #"errordict"
    #"$error"
    #"systemdict"
    #"userdict"
    #"globaldict"
    #"statusdict"
    #"countdictstack"
    #"dictstack"
    #"cleardictstack"

    ;; String Operators
    #"string"
    ;; length, get, put, getinterval, putinterval, copy, forall
    #"anchorsearch"
    #"search"
    #"token"

    ;; Relational, Boolean and Bitwise Operators
    #"eq"
    #"ne"
    #"ge"
    #"gt"
    #"le"
    #"lt"
    #"and"
    #"not"
    #"or"
    #"xor"
    #"true"
    #"false"
    #"bitshift"

    ;; Control Operators
    #"exec"
    #"if"
    #"ifelse"
    #"for"
    #"repeat"
    #"loop"
    #"exit"
    #"stop"
    #"stopped"
    #"countexecstack"
    #"execstack"
    #"quit"
    #"start"

    ;; Type, Attribute, and Conversion Operators
    #"type"
    #"cvlit"
    #"cvx"
    #"xcheck"
    #"executeonly"
    #"noaccess"
    #"readonly"
    #"rcheck"
    #"wcheck"
    #"cvi"
    #"cvn"
    #"cvr"
    #"cvrs"
    #"cvs"

    ;; File Operators
    #"file"
    #"filter"
    #"closefile"
    #"read"
    #"write"
    #"readhexstring"
    #"writehexstring"
    #"readstring"
    #"writestring"
    #"readline"
    ;; token
    #"bytesavailable"
    #"flush"
    #"flushfile"
    #"resetfile"
    #"status"
    #"run"
    #"currentfile"
    #"deletefile"
    #"renamefile"
    #"filenameforall"
    #"setfileposition"
    #"fileposition"
    #"print"
    #"="
    #"=="
    #"stack"
    #"pstack"
    #"printobject"
    #"writeobject"
    #"setobjectformat"
    #"currentobjectformat"

    ;; Resource Operators
    #"defineresource"
    #"undefineresource"
    #"findresource"
    #"resourcestatus"
    #"resourceforall"

    ;; Virtual Memory Operators
    #"save"
    #"restore"
    #"setglobal"
    #"currentglobal"
    #"gcheck"
    #"startjob"
    #"defineuserobject"
    #"execuserobject"
    #"undefineuserobject"
    #"UserObjects"

    ;; Miscellaneous Operators
    #"bind"
    #"null"
    #"version"
    #"realtime"
    #"usertime"
    #"languagelevel"
    #"product"
    #"revision"
    #"serialnumber"
    #"executive"
    #"echo"
    #"prompt"

    ;; Graphics State Operators -- Device Independent
    #"gsave"
    #"grestore"
    #"grestoreall"
    #"initgraphics"
    #"gstate"
    #"setgstate"
    #"currentgstate"
    #"setlinewidth"
    #"currentlinewidth"
    #"setlinecap"
    #"currentlinecap"
    #"setlinejoin"
    #"setmiterlimit"
    #"currentmiterlimit"
    #"setstrokeadjust"
    #"currentstrokeadjust"
    #"setdash"
    #"currentdash"
    #"setcolorspace"
    #"currentcolorspace"
    #"setcolor"
    #"currentcolor"
    #"setgray"
    #"currentgray"
    #"sethsbcolor"
    #"currenthsbcolor"
    #"setrgbcolor"
    #"currentrgbcolor"
    #"setcmykcolor"
    #"currentcmykcolor"

    ;; Graphics State Operators -- Device Dependant
    #"sethalftone"
    #"currenthalftone"
    #"setscreen"
    #"currentscreen"
    #"setcolorscreen"
    #"currentcolorscreen"
    #"settransfer"
    #"currenttransfer"
    #"setcolorstransfer"
    #"currentcolortransfer"
    #"setblackgeneration"
    #"currentblackgeneration"
    #"setundercolorremoval"
    #"currentundercolorremoval"
    #"setcolorrendering"
    #"currentcolorrendering"
    #"setflat"
    #"currentflat"
    #"setoverprint"
    #"currentoverprint"

    ;; Coordinate System and Matrix Operators
    #"matrix"
    #"initmatrix"
    #"identmatrix"
    #"defaultmatrix"
    #"currentmatrix"
    #"setmatrix"
    #"translate"
    #"scale"
    #"rotate"
    #"concat"
    #"concatmatrix"
    #"transform"
    #"dtransform"
    #"itransform"
    #"idtransform"
    #"invertmatrix"

    ;; Path Construction Operators
    #"newpath"
    #"currentpoint"
    #"moveto"
    #"rmoveto"
    #"lineto"
    #"rlineto"
    #"arc"
    #"arcn"
    #"arct"
    #"arcto"
    #"curveto"
    #"rcurveto"
    #"closepath"
    #"flattenpath"
    #"reversepath"
    #"strokepath"
    #"ustrokepath"
    #"charpath"
    #"uappend"
    #"clippath"
    #"setbbox"
    #"pathbbox"
    #"pathforall"
    #"upath"
    #"initclip"
    #"clip"
    #"eoclip"
    #"rectclip"
    #"ucache"

    ;; Painting Operators
    #"erasepage"
    #"fill"
    #"eofill"
    #"stroke"
    #"ufill"
    #"ueofill"
    #"ustroke"
    #"rectfill"
    #"rectstroke"
    #"image"
    #"colorimage"
    #"imagemask"

    ;; Insideness Testing Operators
    #"infill"
    #"ineofill"
    #"inufill"
    #"inueofill"
    #"instroke"
    #"inustroke"

    ;; Form and Pattern Operators
    #"makepattern"
    #"setpattern"
    #"execform"

    ;; Device Setup and Output Operators
    #"showpage"
    #"copypage"
    #"setpagedevice"
    #"currentpagedevice"
    #"nulldevice"

    ;; Character and Font Operators
    #"definefont"
    #"undefinefont"
    #"findfont"
    #"scalefont"
    #"makefont"
    #"setfont"
    #"currentfont"
    #"rootfont"
    #"selectfont"
    #"show"
    #"ashow"
    #"widthshow"
    #"awidthshow"
    #"xshow"
    #"xyshow"
    #"yshow"
    #"glyphshow"
    #"stringwidth"
    #"cshow"
    #"kshow"
    #"FontDirectory"
    #"GlobalFontDirectory"
    #"StandardEncoding"
    #"ISOLatinEncoding"
    #"findencoding"
    #"setcachedevice"
    #"setcachedevice2"
    #"setcharwidth"

    ;; Interpreter Parameter Operators
    #"setsystemparams"
    #"currentsystemparams"
    #"setuserparams"
    #"currentuserparams"
    #"setdevparams"
    #"currentdevparams"
    #"vmreclaim"
    #"setvmthreshold"
    #"vmstatus"
    #"cachestatus"
    #"setcachelimit"
    #"setcacheparams"
    #"currentcacheparams"
    #"setucacheparams"
    #"ucachestatus"

    ;; Display PostScript Operators
    ;; currentcontext, fork, join, detach, lock, monitor,
    ;; condition, wait, notify, yield, defineusername,
    ;; viewclip, eoviewclip, rectviewclip, initviewclip,
    ;; viewclippath, deviceinfo, wtranslation,
    ;; sethalftonephase, currenthalftonephase

    ;; Errors
    #"handleerror"
    ;; configurationerror, dictfull, dictstackoverflow, dictstackunderflow,
    ;; execstackoverflow, interrupt, invalidaccess, invalidcontext, invalidexit,
    ;; invalidfileaccess, invalidfont, invalidid, invalidrestore, ioerror,
    ;; limitcheck, nocurrentpoint, rangecheck, stackoverflow, stackunderflow,
    ;; syntaxerror, timeout, typecheck, undefined, undefinedfilename,
    ;; undefinedresource, undefinedresult, unmatchedmark, unregistered,
    ;; VMerror
    ))
    
;;;# INTERPRETER

(defclass interpreter ()
  ((%operand-stack
    :accessor operand-stack
    :initform '())
   (%dictionary-stack
    :accessor dictionary-stack
    :initform '())
   (%execution-stack
    :accessor execution-stack
    :initform '())
   (%gs-stack
    :accessor graphics-state-stack
    :initform '())))

(defparameter *interpreter* nil)

(defun lookup (name)
  (loop with bytes = (object-value name)
	for dict in (dictionary-stack *interpreter*)
	for value = (gethash bytes (object-value dict))
	when value
	  do (return value)
	finally (error "Undefined: ~A" name)))

(defparameter *systemdict* (make-ps-dictionary))

(defparameter *globaldict* (make-ps-dictionary))

(defparameter *userdict* (make-ps-dictionary))

(defun make-interpreter ()
  (serapeum:lret ((interpreter (make-instance 'interpreter)))
    (push *systemdict* (dictionary-stack interpreter))
    (push *globaldict* (dictionary-stack interpreter))
    (push *userdict* (dictionary-stack interpreter))
    (dict-def *systemdict* #"true" +true+)
    (dict-def *systemdict* #"false" +false+)
    (dict-def *systemdict* #"systemdict" *systemdict*)
    (dict-def *systemdict* #"globaldict" *globaldict*)
    (dict-def *systemdict* #"userdict" *userdict*)
    (dict-def *systemdict* #"exit" :exit)))

(defclass looping-context ()
  ((%proc
    :initarg :procedure
    :reader looping-context-procedure)))

(defclass for-context (looping-context)
  ((%control
    :initarg :control
    :accessor for-context-control)
   (%limit
    :initarg :limit
    :reader for-context-limit)
   (%increment
    :initarg :increment
    :reader for-context-increment)))

(defun limit-test (for-context)
  (if (plusp (for-context-increment for-context))
      #'>
      #'<))

(defgeneric update-looping-context (looping-context)
  (:method ((for-context for-context))
    (with-accessors ((procedure looping-context-procedure)
		     (control for-context-control)
		     (limit for-context-limit)
		     (increment for-context-increment))
	for-context
      (unless (funcall (limit-test for-context) control limit)
	(push (make-number control) (operand-stack *interpreter*))
	(incf control increment)
	(push for-context (execution-stack *interpreter*))
	(push procedure (execution-stack *interpreter*))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defmacro defoperator (name &body body)
  `(dict-def
    *systemdict* ,name
    (make-instance
     'ps-operator
     :value ,name
     :function (lambda ()
		 (with-accessors ((operand-stack operand-stack)
				  (dictionary-stack dictionary-stack)
				  (execution-stack execution-stack)
				  (graphics-state-stack graphics-state-stack))
		     *interpreter*
		   ,@body)))))

(defmacro stackunderflow (length)
  `(unless (>= (length operand-stack) ,length)
     (error "Stackunderflow")))

(defmacro typecheck (&rest types)
  `(unless (and ,@(loop for index from (1- (length types)) downto 0
			for typename in types
			collecting `(typep (nth ,index operand-stack) ',typename)))
     (error "Typecheck"))))

(defoperator #"["
  (push +mark+ operand-stack))

(defoperator #"<<"
  (push +mark+ operand-stack))

(defoperator #"mark"
  (push +mark+ operand-stack))

(defoperator #"]"
  (loop for element = (pop operand-stack)
	until (eq +mark+ element)
	collecting element into result
	when (null operand-stack)
	  do (error "Unmatchedmark")
	finally (push (make-ps-array (nreverse result)) operand-stack)))

(defoperator #">>"
  (let ((dict (make-ps-dictionary))
	(keys-and-values
	  (loop for element = (pop operand-stack)
		until (eq +mark+ element)
		collecting element into result
		when (null operand-stack)
		  do (error "Unmatchedmark")
		finally (return (nreverse result)))))
    (unless (evenp (length keys-and-values))
      (error "Rangecheck"))
    (loop for (key value) on keys-and-values by #'cddr
	  do (dict-def dict key value))
    (push dict operand-stack)))

(defoperator #"="
  (stackunderflow 1)
  (let ((any (pop operand-stack)))
    (princ (octets-latin1 (convert-to-string any)) *standard-output*)
    (terpri *standard-output*)))

(defoperator #"add"
  (stackunderflow 2)
  (typecheck ps-number ps-number)
  (let* ((num2 (pop operand-stack))
	 (num1 (pop operand-stack))
	 (sum (+ (object-value num1) (object-value num2))))
    (if (integerp sum)
	(push (make-instance 'ps-integer :value sum) operand-stack)
	(push (make-instance 'ps-real :value (float sum 1d0)) operand-stack))))

(defoperator #"array"
  (stackunderflow 1)
  (typecheck ps-integer)
  (push (make-ps-array (loop repeat (object-value (pop operand-stack)) collect +null+))
	operand-stack))

(defoperator #"begin"
  (stackunderflow 1)
  (typecheck ps-dictionary)
  (push (pop operand-stack) dictionary-stack))

(defoperator #"cleardictstack"
  (loop for dict = (first dictionary-stack)
	until (eq dict *userdict*)
	do (pop dictionary-stack)))

(defoperator #"cleartomark"
  (loop for element = (pop operand-stack)
	until (eq +mark+ element)
	when (null operand-stack)
	  do (error "Unmatchedmark")))

(defoperator #"copy"
  (stackunderflow 2)
  (if (typep (first operand-stack) 'ps-integer)
      (let* ((n (pop operand-stack))
	     (n-value (object-value n)))
	(when (> n-value (length operand-stack))
	  (error "Rangecheck"))
	(loop for i downfrom (1- n-value) to 0
	      do (push (nth i operand-stack) operand-stack)))
      (let ((object2 (pop operand-stack))
	    (object1 (pop operand-stack)))
	(cond ((or (and (typep object1 'ps-string)
			(typep object2 'ps-string))
		   (and (typep object1 'ps-array)
			(typep object1 'ps-array)
			(not (typep object2 'ps-packed-array))))
	       (unless (>= (length (object-value object2))
			   (length (object-value object1)))
		 (error "Rangecheck"))
	       (loop for item across (object-value object1)
		     for index from 0
		     do (setf (aref (object-value object2) index) item))
	       (push object2 operand-stack))
	      ((and (typep object1 'ps-dictionary)
		    (typep object2 'ps-dictionary))
	       (loop for key being each hash-key of (object-value object1)
		       using (hash-value value)
		     do (dict-def (object-value object2) key value))
	       (push object2 operand-stack))
	      ;; FIXME add graphics-state
	      (t (error "Typecheck"))))))

(defoperator #"count"
  (push (length operand-stack) operand-stack))

(defoperator #"countdictstack"
  (push (length dictionary-stack) operand-stack))

(defoperator #"countexecstack"
  (push (length execution-stack) operand-stack))

(defoperator #"counttomark"
  (let ((count (position +mark+ operand-stack)))
    (unless count
      (error "Unmatchedmark"))
    (push count operand-stack)))

(defoperator #"currentdict"
  (push (first dictionary-stack) operand-stack))

(defoperator #"currentfile"
  (push (find-if 'streamp execution-stack) operand-stack)) ;; FIXME !!!

(defgeneric convert-to-string (thing)
  (:method ((thing ps-object))
    #"--nostring-val")
  (:method ((thing ps-boolean))
    (latin1-octets (object-value thing)))
  (:method ((thing ps-string))
    (object-value thing))
  (:method ((thing ps-number))
    (latin1-octets (princ-to-string (object-value thing))))
  (:method ((thing ps-name))
    (object-value thing))
  (:method ((thing ps-operator))
    (object-value thing)))
		   
(defoperator #"cvs"
  (stackunderflow 2)
  (typecheck ps-string ps-object)
  (let ((string (pop operand-stack))
	(any (pop operand-stack)))
    (let ((repr (convert-to-string any)))
      (unless (>= (length repr) (length string)) ;; FIXME do we bother?
	(error "Rangecheck"))
      (setf (object-value string) repr)
      (push string operand-stack))))

(defoperator #"cvx"
  (stackunderflow 1)
  (setf (object-executable-p (first operand-stack)) t))

(defoperator #"def"
  (stackunderflow 2)
  (let ((value (pop operand-stack))
	(key (pop operand-stack)))
    (dict-def (first dictionary-stack) key value)))

(defoperator #"dict"
  (stackunderflow 1)
  (typecheck ps-integer)
  (pop operand-stack)
  (push (make-ps-dictionary) operand-stack))

(defoperator #"div"
  (stackunderflow 2)
  (typecheck ps-number ps-number)
  (let ((num2 (pop operand-stack))
	(num1 (pop operand-stack)))
    (push (make-instance 'ps-real :value (float (/ num1 num2) 1d0))
	  operand-stack)))

(defoperator #"dup"
  (stackunderflow 1)
  (push (first operand-stack) operand-stack))

;; eexec

(defoperator #"end"
  (when (eq *userdict* (first dictionary-stack))
    (error "Dictstackunderflow"))
  (pop dictionary-stack))

(defoperator #"exch"
  (stackunderflow 2)
  (let ((any2 (pop operand-stack))
	(any1 (pop operand-stack)))
    (push any2 operand-stack)
    (push any1 operand-stack)))

(defoperator #"exec"
  (stackunderflow 1)
  (push (pop operand-stack) execution-stack))

(defoperator #"executeonly"
  (stackunderflow 1)
  (typecheck ps-composite-object) ;; FIXME
  (setf (object-access (first operand-stack)) :execute-only))

(defoperator #"exit"
  (let ((lc (find 'looping-context execution-stack :key #'type-of)))
    (unless lc
      (error "Invalidexit"))
    (loop for item = (pop execution-stack)
	  until (eq item lc))))

(defun make-number (number)
  (assert (numberp number))
  (if (integerp number)
      (make-instance 'ps-integer :value number)
      (make-instance 'ps-real :value (float number 1d0))))

(defoperator #"for"
  (stackunderflow 4)
  (typecheck ps-number ps-number ps-number ps-procedure)
  (let ((proc (pop operand-stack))
	(limit (object-value (pop operand-stack)))
	(increment (object-value (pop operand-stack)))
	(initial (object-value (pop operand-stack))))
    (let ((fc (make-instance 'for-context
			     :procedure proc
			     :control initial
			     :limit limit
			     :increment increment)))
      (push fc execution-stack))))

(defoperator #"get"
  (stackunderflow 2)
  (let ((key (pop operand-stack))
	(object (pop operand-stack)))
    (cond ((or (typep object 'ps-array)
	       (typep object 'ps-string))
	   (let ((length (length object)))
	     (unless (typep key 'ps-integer)
	       (error "Typecheck"))
	     (let* ((index (object-value key)))
	       (when (>= index length)
		 (error "Rangecheck"))
	       (nth index (object-value object)))))
	  ((typep object 'ps-dictionary)
	   (multiple-value-bind (value found)
	       (gethash key (object-value object))
	     (if found
		 (push value operand-stack)
		 (error "Undefined"))))
	  (t (error "Typecheck")))))

;; NP
;; internaldict***
;; startlock startlck
;; definefont

(defoperator #"if"
  (stackunderflow 2)
  (typecheck ps-procedure ps-boolean)
  (let ((bool (pop operand-stack))
	(proc (pop operand-stack)))
    (if (eq +true+ bool)
	(push proc execution-stack))))

(defoperator #"ifelse"
  (stackunderflow 3)
  (typecheck ps-procedure ps-procedure ps-boolean)
  (let ((proc2 (pop operand-stack))
	(proc1 (pop operand-stack))
	(bool (pop operand-stack)))
    (if (eq +true+ bool)
	(push proc1 execution-stack)
	(push proc2 execution-stack))))

(defoperator #"index"
  (stackunderflow 2)
  (typecheck ps-integer)
  (let* ((n (pop operand-stack))
	 (n-value (object-value n)))
    (when (or (minusp n-value)
	      (>= (length operand-stack) n-value))
      (error "Rangecheck"))
    (push (nth n-value operand-stack) operand-stack)))

(defoperator #"internaldict"
  (stackunderflow 1)
  (let ((int (pop operand-stack)))
    (unless (and (typep int 'ps-integer)
		 (= (object-value int) 1183615869)) ;; MAGIC
      (error "Undefined"))
    (push *internaldict* operand-stack))) ;; FIXME - WHAT IS IN INTERNALDICT that fonts care about?

(defoperator #"known"
  (stackunderflow 2)
  (typecheck ps-dictionary ps-object)
  (let ((key (pop operand-stack))
	(dict (pop operand-stack)))
    (multiple-value-bind (value found)
	(gethash key (object-value dict))
      (declare (ignore value))
      (if found
	  (push +true+ operand-stack)
	  (push +false+ operand-stack)))))

(defoperator #"lt"
  (stackunderflow 2)
  (let ((object2 (pop operand-stack))
	(object1 (pop operand-stack)))
    (cond ((and (typep object1 'ps-number)
		(typep object2 'ps-number))
	   (if (< (object-value object1) (object-value object2))
	       (push +true+ operand-stack)
	       (push +false+ operand-stack)))
	  ((and (typep object1 'ps-string)
		(typep object2 'ps-string))
	   (if (octets< (object-value operand1) (object-value object2)) ;; FIXME implement this
	       (push +true+ operand-stack)
	       (push +false+ operand-stack)))
	  (t (error "Typecheck")))))

(defoperator #"noaccess"
  (stackunderflow 1)
  (typecheck ps-composite-object) ;; FIXME
  (setf (object-access (first operand-stack)) :no-access))

(defoperator #"pop"
  (stackunderflow 1)
  (pop operand-stack))

(defoperator #"put"
  (stackunderflow 3)
  (let ((value (pop operand-stack))
	(key (pop operand-stack))
	(object (pop operand-stack)))
    (cond ((or (typep object 'ps-array)
	       (typep object 'ps-string))
	   (let ((length (length object)))
	     (unless (typep key 'ps-integer)
	       (error "Typecheck"))
	     (let* ((index (object-value key)))
	       (when (>= index length)
		 (error "Rangecheck"))
	       (when (and (typep object 'ps-string)
			  (not (typep value 'ps-integer)))
		 (error "Typecheck"))
	       (setf (nth index (object-value object)) value))))
	  ((typep object 'ps-dictionary)
	   (dict-def object key value))
	  (t (error "Typecheck")))))

(defoperator #"readonly"
  (stackunderflow 1)
  (typecheck ps-composite-object)
  (setf (object-access (first operand-stack)) :read-only))

;;;# NEXT OBJECT

(defun next-object ()
  (when (not (null (execution-stack *interpreter*)))
    (let* ((source (first (execution-stack *interpreter*)))
	   (next
	     (cond ((typep source 'ps-string)
		    (let ((string (pop (execution-stack *interpreter*))))
		      (push (make-octet-vector-stream (object-value string))
			    (execution-stack *interpreter*))
		      (next-object)))
		   ((typep source 'ps-procedure)
		    (let ((proc (pop (execution-stack *interpreter*))))
		      (dolist (item (reverse (object-value proc)))
			(push item (execution-stack *interpreter*)))
		      (next-object)))
		   ((typep source 'ps-object)
		    (pop (execution-stack *interpreter*)))
		   ((typep source 'looping-context)
		    (pop (execution-stack *interpreter*)))
		   ((streamp source) ;; FIXME ps-file
		    (let* ((*ps-stream* source)
			   (obj (read-object nil)))
		      (if (eql :eof obj)
			  (progn (pop (execution-stack *interpreter*))
				 (next-object))
			  obj)))
		   (t (error "Unexpected object on execution stack: ~A" source)))))
      (if next next (progn (pop (execution-stack *interpreter*)) (next-object))))))

(defun literalp (object)
  (not (object-executable-p object)))

(defun interpret ()
  (with-accessors ((operand-stack operand-stack)
		   (dictionary-stack dictionary-stack)
		   (execution-stack execution-stack)
		   (graphics-state-stack graphics-state-stack))
      *interpreter*	
    (loop for object = (next-object)
	  while object
	  do (dump-stack)
	     (cond
	       ((typep object 'looping-context)
		(update-looping-context object))
	       ((literalp object)
		(push object operand-stack))
	       ((typep object 'ps-name)
		(format t "~%~A" (octets-latin1 (object-value object))) ;; DEBUG
		(let ((value (lookup object)))
		  (cond
		    ((typep value 'ps-operator)
		     (funcall (operator-function value)))
		    ((and (typep value 'ps-object)
			  (literalp value))
		     (push value operand-stack))
		    (t (warn "Unhandled value: ~A" object)))))
	       ((typep object 'ps-procedure)
		(push object operand-stack)) ;; because at top level
	       (t (warn "Unhandled object: ~A" object))))))

(defun interpret-file (pathname)
  (with-input-from-octet-file (s pathname)
    (let ((*interpreter* (make-interpreter)))
      (push s (execution-stack *interpreter*))
      (interpret))))

(defun interpret-octets (octets)
  (with-input-from-octet-vector (s octets)
    (let ((*interpreter* (make-interpreter)))
      (push s (execution-stack *interpreter*))
      (interpret))))

(defun dump-stack ()
  (format t "~%<~{ ~A ~}>" (mapcar 'type-of (operand-stack *interpreter*))))
