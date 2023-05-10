;;;; jqs-type1

(in-package #:com.splittist.type1)

(named-readtables:in-readtable syntax)
    
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

(defclass pfa-interpreter (interpreter)
  ((%fontdefinedp
    :accessor font-defined-p
    :initform nil)))

(defparameter *interpreter* nil)

(defun lookup (name)
  (loop for dict in (dictionary-stack *interpreter*)
	for value = (dict-get dict name)
	when value
	  do (return value)
	finally (error "Undefined: ~A" name)))

(defparameter *systemdict* (make-ps-dictionary))

(defparameter *globaldict* (make-ps-dictionary))

(defparameter *userdict* (make-ps-dictionary))

(defparameter *standard-encoding*
  #(#".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef"
    #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef"
    #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef"
    #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef"
    #"space" #"exclam" #"quotedbl" #"numbersign"
    #"dollar" #"percent" #"ampersand" #"quoteright"
    #"parenleft" #"parenright" #"asterisk" #"plus"
    #"comma" #"hyphen" #"period" #"slash"
    #"zero" #"one" #"two" #"three"
    #"four" #"five" #"six" #"seven"
    #"eight" #"nine" #"colon" #"semicolon"
    #"less" #"equal" #"greater" #"question"
    #"at" #"A" #"B" #"C" #"D" #"E" #"F" #"G"
    #"H" #"I" #"J" #"K" #"L" #"M" #"N" #"O"
    #"P" #"Q" #"R" #"S" #"T" #"U" #"V" #"W"
    #"X" #"Y" #"Z" #"bracketleft" #"backslash" #"bracketright" #"asciicircum" #"underscore"
    #"quoteleft" #"a" #"b" #"c" #"d" #"e" #"f" #"g"
    #"h" #"i" #"j" #"k" #"l" #"m" #"n" #"o"
    #"p" #"q" #"r" #"s" #"t" #"u" #"v" #"w"
    #"x" #"y" #"z" #"braceleft" #"bar" #"braceright" #"asciitilde" #".notdef"
    #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef"
    #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef"
    #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef"
    #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef"
    #".notdef" #"exclamdown" #"cent" #"sterling"
    #"fraction" #"yen" #"florin" #"section"
    #"currency" #"quotesingle" #"quotedblleft" #"guillemotleft"
    #"guilsinglleft" #"guilsinglright" #"fi" #"fl"
    #".notdef" #"endash" #"dagger" #"daggerdbl"
    #"periodcentered" #".notdef" #"paragraph" #"bullet"
    #"quotesinglbase" #"quotedblbase" #"quotedblright" #"guillemotright"
    #"ellipsis" #"perthousand" #".notdef" #"questiondown"
    #".notdef" #"grave" #"acute" #"circumflex" #"tilde" #"macron" #"breve" #"dotaccent"
    #"dieresis" #".notdef" #"ring" #"cedilla" #".notdef" #"hungarumlaut" #"ogonek" #"caron"
    #"emdash" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef"
    #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef"
    #".notdef" #"AE" #".notdef" #"ordfeminine" #".notdef" #".notdef" #".notdef" #".notdef"
    #"Lslash" #"Oslash" #"OE" #"ordmasculine" #".notdef" #".notdef" #".notdef" #".notdef"
    #".notdef" #"ae" #".notdef" #".notdef" #".notdef" #"dotlessi" #".notdef" #".notdef"
    #"lslash" #"oslash" #"oe" #"germandbls" #".notdef" #".notdef" #".notdef" #".notdef"))

(defparameter *iso-latin1-encoding*
  #(#".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef"
    #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef"
    #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef"
    #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef"
    #"space" #"exclam" #"quotedbl" #"numbersign"
    #"dollar" #"percent" #"ampersand" #"quoteright"
    #"parenleft" #"parenright" #"asterisk" #"plus"
    #"comma" #"minus" #"period" #"slash"
    #"zero" #"one" #"two" #"three"
    #"four" #"five" #"six" #"seven"
    #"eight" #"nine" #"colon" #"semicolon"
    #"less" #"equal" #"greater" #"question"
    #"at" #"A" #"B" #"C" #"D" #"E" #"F" #"G"
    #"H" #"I" #"J" #"K" #"L" #"M" #"N" #"O"
    #"P" #"Q" #"R" #"S" #"T" #"U" #"V" #"W"
    #"X" #"Y" #"Z" #"bracketleft" #"backslash" #"bracketright" #"asciicircum" #"underscore"
    #"quoteleft" #"a" #"b" #"c" #"d" #"e" #"f" #"g"
    #"h" #"i" #"j" #"k" #"l" #"m" #"n" #"o"
    #"p" #"q" #"r" #"s" #"t" #"u" #"v" #"w"
    #"x" #"y" #"z" #"braceleft" #"bar" #"braceright" #"asciitilde" #".notdef"
    #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef"
    #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef" #".notdef"
    #"dotlessi" #"grave" #"acute" #"circumflex" #"tilde" #"macron" #"breve" #"dotaccent"
    #"dieresis" #".notdef" #"ring" #"cedilla" #".notdef" #"hungarumlaut" #"ogonek" #"caron"
    #"space" #"exclamdown" #"cent" #"sterling"
    #"currency" #"yen" #"brokenbar" #"section"
    #"dieresis" #"copyright" #"ordfeminine" #"guillemotleft"
    #"logicalnot" #"hyphen" #"registered" #"macron"
    #"degree" #"plusminus" #"twosuperior" #"threesuperior"
    #"acute" #"mu" #"paragraph" #"periodcentered"
    #"cedilla" #"onesuperior" #"ordmasculine" #"guillemotright"
    #"onequarter" #"onehalf" #"threequarters" #"questiondown"
    #"Agrave" #"Aacute" #"Acircumflex" #"Atilde"
    #"Adieresis" #"Aring" #"AE" #"Ccedilla"
    #"Egrave" #"Eacute" #"Ecircumflex" #"Edieresis"
    #"Igrave" #"Iacute" #"Icircumflex" #"Idieresis"
    #"Eth" #"Ntilde" #"Ograve" #"Oacute"
    #"Ocircumflex" #"Otilde" #"Odieresis" #"multiply"
    #"Oslash" #"Ugrave" #"Uacute" #"Ucircumflex"
    #"Udieresis" #"Yacute" #"Thorn" #"germandbls"
    #"agrave" #"aacute" #"acircumflex" #"atilde"
    #"adieresis" #"aring" #"ae" #"ccedilla"
    #"egrave" #"eacute" #"ecircumflex" #"edieresis"
    #"igrave" #"iacute" #"icircumflex" #"idieresis"
    #"eth" #"ntilde" #"ograve" #"oacute"
    #"ocircumflex" #"otilde" #"odieresis" #"divide"
    #"oslash" #"ugrave" #"uacute" #"ucircumflex"
    #"udieresis" #"yacute" #"thorn" #"ydieresis"))

(defun make-encoding-vector (vector-of-names)
  (make-ps-array
   (map 'list 'make-name vector-of-names)))

(defun make-interpreter ()
  (serapeum:lret ((interpreter (make-instance 'pfa-interpreter)))
    (push *systemdict* (dictionary-stack interpreter))
    (push *globaldict* (dictionary-stack interpreter))
    (push *userdict* (dictionary-stack interpreter))
    (dict-def *systemdict* (make-name #"true" t) +true+)
    (dict-def *systemdict* (make-name #"false" t) +false+)
    (dict-def *systemdict* (make-name #"systemdict" t) *systemdict*)
    (dict-def *systemdict* (make-name #"globaldict" t) *globaldict*)
    (dict-def *systemdict* (make-name #"userdict" t) *userdict*)
    (dict-def *systemdict* (make-name #"FontDirectory" t) (make-ps-dictionary))
    (dict-def *systemdict* (make-name #"StandardEncoding" t) (make-encoding-vector *standard-encoding*))
    (dict-def *systemdict* (make-name #"ISOLatin1Encoding" t) (make-encoding-vector *iso-latin1-encoding*))))

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
    *systemdict* (make-name ,name t)
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

(defoperator #"closefile"
  (stackunderflow 1)
  (unless (streamp (nth 0 operand-stack))
    (error "Typecheck"))
  (let ((stream (pop operand-stack))) ;; FIXME
    (setf execution-stack (remove stream execution-stack))
    (close stream)))

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
    nil)
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
    (let ((repr (or (convert-to-string any) #"--nostringval--")))
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

(defoperator #"definefont"
  (stackunderflow 2)
  (typecheck ps-dictionary)
  ;; only for font definition files
  (let ((font (pop operand-stack))
	(key (pop operand-stack)))
    (dict-def (lookup #"FontDirectory") key font)
    (setf (font-defined-p *interpreter*) t)))

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
  (push (first operand-stack) operand-stack)) ;; FIXME really should duplicate

(defoperator #"eexec"
  (stackunderflow 1)
  (let ((source (pop operand-stack)))
    (cond ((streamp source)
	   ;;(read-byte source)
	   (let ((eexec-stream (make-eexec-stream source)))
	     (push eexec-stream execution-stack)
	     (push *systemdict* dictionary-stack)))
	  ((typep source 'ps-string)
	   (let ((string-stream (make-octet-vector-stream (object-value source))))
	     (push (make-eexec-stream string-stream) execution-stack)
	     (push *systemdict* dictionary-stack)))
	  (t
	   (error "Typecheck")))))

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
  (let ((lc (find-if (lambda (item) (typep item 'looping-context)) execution-stack)))
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
	       (dict-get object key)
	     (if found
		 (push value operand-stack)
		 (error "Undefined"))))
	  (t (error "Typecheck")))))

(defoperator #"if"
  (stackunderflow 2)
  (typecheck ps-boolean ps-procedure)
  (let ((proc (pop operand-stack))
	(bool (pop operand-stack)))
    (if (eq +true+ bool)
	(push proc execution-stack))))

(defoperator #"ifelse"
  (stackunderflow 3)
  (typecheck ps-boolean ps-procedure ps-procedure)
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
	      (>= n-value (length operand-stack)))
      (error "Rangecheck"))
    (push (nth n-value operand-stack) operand-stack)))

#+(or)(defoperator #"internaldict"
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
	(dict-get dict key)
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
	   (if (octets< (object-value object1) (object-value object2)) ;; FIXME implement this
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
	   (let ((length (length (object-value object))))
	     (unless (typep key 'ps-integer)
	       (error "Typecheck"))
	     (let* ((index (object-value key)))
	       (when (>= index length)
		 (error "Rangecheck"))
	       (when (and (typep object 'ps-string)
			  (not (typep value 'ps-integer)))
		 (error "Typecheck"))
	       (setf (elt (object-value object) index) value))))
	  ((typep object 'ps-dictionary)
	   (dict-def object key value))
	  (t (error "Typecheck")))))

(defoperator #"readonly"
  (stackunderflow 1)
  (typecheck ps-composite-object)
  (setf (object-access (first operand-stack)) :read-only))

(defoperator #"readstring"
  (stackunderflow 2)
  (unless (and (streamp (nth 1 operand-stack))
	       (typep (nth 0 operand-stack) 'ps-string))
    (error "Typecheck"))
  (let* ((string (pop operand-stack))
	 (file (pop operand-stack))
	 (value (object-value string))
	 (length (length value))
	 (eof nil))
    (read-byte file nil)
    (loop for index below length
	  for byte = (read-byte file nil)
	  while byte
	  do (setf (aref value index) byte)
	  finally (unless byte
		    (setf (fill-pointer value) index
			  eof t)))
    (push string operand-stack)
    (push (if eof +false+ +true+) operand-stack)))

(defoperator #"string"
  (stackunderflow 1)
  (typecheck ps-integer)
  (let ((length (object-value (pop operand-stack))))
    (unless (plusp length)
      (error "Rangecheck"))
    (let ((string (make-instance 'ps-string
				 :value (make-array length
						    :element-type '(unsigned-byte 8)
						    :initial-element 0))))
      (push string operand-stack))))

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

(defun interpret-pfa ()
  (with-accessors ((operand-stack operand-stack)
		   (execution-stack execution-stack)
		   (font-defined-p font-defined-p))
      *interpreter*	
    (loop for object = (next-object)
	  while object
	  until font-defined-p
	  do ;; (dump-stack) ;; DEBUG
	     (cond
	       ((typep object 'looping-context)
		(update-looping-context object))
	       ((literalp object)
		(push object operand-stack))
	       ((typep object 'ps-name)
		;; (format t "~%~A" (octets-latin1 (object-value object))) ;; DEBUG
		(let ((value (lookup object)))
		  (cond
		    ((typep value 'ps-operator)
		     (funcall (operator-function value)))
		    ((and (typep value 'ps-object)
			  (not (literalp value)))
		     (push value execution-stack))
		    ((and (typep value 'ps-object)
			  (literalp value))
		     (push value operand-stack))
		    (t (warn "Unhandled value: ~A" object)))))
	       ((typep object 'ps-procedure)
		(push object operand-stack)) ;; because at top level
	       (t (warn "Unhandled object: ~A" object)))
	  finally (return (lookup #"FontDirectory")))))

(defun interpret-file (pathname)
  (with-input-from-octet-file (s pathname)
    (when (= +pfb-marker+ (peek-byte s))
      (file-position s 0)
      (setf s (make-pfb-stream s)))
    (let ((*interpreter* (make-interpreter)))
      (push s (execution-stack *interpreter*))
      (interpret-pfa))))

(defun interpret-octets (octets)
  (with-input-from-octet-vector (s octets)
    (let ((*interpreter* (make-interpreter)))
      (push s (execution-stack *interpreter*))
      (interpret-pfa))))

(defun dump-stack ()
  (format t "~%<~{ ~A ~}>"
	  (mapcar
	   (lambda (item)
	     (if (typep item 'ps-object)
		 (alexandria:if-let ((octets (convert-to-string item)))
		   (octets-latin1 octets)
		   (type-of item))
		 (type-of item)))
	   (operand-stack *interpreter*))))

