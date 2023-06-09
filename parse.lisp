;;;; jqs-type1

(in-package #:com.splittist.type1)

(named-readtables:in-readtable syntax)

;;;# OBJECTS

(defclass ps-object ()
  ((%value
    :accessor object-value
    :initarg :value
    :initform nil)
   (%executablep
    :accessor object-executable-p
    :initarg :executablep)))

(defmethod print-object ((object ps-object) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (alexandria:when-let ((value (object-value object)))
      (princ value))))

;;# SIMPLE OBJECTS

(defclass ps-simple-object (ps-object)
  ())

(defclass ps-number (ps-simple-object)
  ()
  (:default-initargs :executablep nil))

(defclass ps-integer (ps-number)
  ())

(defclass ps-real (ps-number)
  ())

(defclass ps-name (ps-simple-object)
  ())

(defun make-name (bytes &optional executablep)
  (make-instance 'ps-name :value bytes :executablep executablep))

(defmethod print-object ((object ps-name) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (alexandria:when-let ((value (object-value object)))
      (princ (octets-latin1 value) stream))))

(defun nameql (name1 name2)
  (when (typep name1 'ps-name)
    (setf name1 (object-value name1)))
  (when (typep name2 'ps-name)
    (setf name2 (object-value name2)))
  (octets= name1 name2))

(defclass ps-mark (ps-simple-object)
  ())

(defparameter +mark+ (make-instance 'ps-mark :value "mark"))

(defclass ps-null (ps-simple-object)
  ()
  (:default-initargs :executablep nil))

(defparameter +null+ (make-instance 'ps-null :value "null"))

(defclass ps-boolean (ps-simple-object)
  ()
  (:default-initargs :executablep nil))

(defparameter +true+ (make-instance 'ps-boolean :value "true"))

(defparameter +false+ (make-instance 'ps-boolean :value "false"))

(defclass ps-operator (ps-simple-object)
  ((%function
    :initarg :function
    :accessor operator-function))
  (:default-initargs :executablep t))

;;# COMPOSITE OBJECTS

(defclass ps-composite-object (ps-object)
  ((%access
    :accessor object-access
    :initarg :access
    :initform :unlimited
    :type (member :unlimited :read-only :execute-only :none))))

(defclass ps-array (ps-composite-object)
  ()
  (:default-initargs :executablep nil))

(defmethod print-object ((object ps-array) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (alexandria:when-let ((value (object-value object)))
      (princ (length value) stream))))

(defun make-ps-array (elements)
  (make-instance 'ps-array :value (apply 'vector elements)))

(defclass ps-packed-array (ps-array)
  ()
  (:default-initargs :access :read-only))

(defclass ps-procedure (ps-array)
  ()
  (:default-initargs :executablep t))

(defclass ps-dictionary (ps-composite-object)
  ()
  (:default-initargs :executablep nil))

(defun make-ps-dictionary ()
  (make-instance 'ps-dictionary :value (make-hash-table :test 'equalp)))

(defun dict-def (dictionary key value)
  (when (typep key 'ps-name)
    (setf key (object-value key)))
  (setf (gethash key (object-value dictionary)) value))

(defun dict-get (dictionary key)
  (when (typep key 'ps-name)
    (setf key (object-value key)))
  (gethash key (object-value dictionary)))

(defclass ps-string (ps-composite-object)
  ()
  (:default-initargs :executablep nil))

(defmethod print-object ((object ps-string) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (alexandria:when-let ((value (object-value object)))
      (princ (octets-latin1 value) stream))))

(defclass ps-file (ps-composite-object)
  ())

;;;# PARSER

(defparameter +white-space+ #(#!Null #!Tab #!Linefeed #!Formfeed #!Return #!Space))

(defun white-space-p (byte)
  (find byte +white-space+))

(defparameter +delimiters+ #"()<>[]{}/%")

(defun delimiterp (byte)
  (find byte +delimiters+))

(defun regular-character-p (byte)
  (and (not (white-space-p byte))
       (not (delimiterp byte))))

(defvar *ps-stream* nil)

(defun eat-char (expected)
  (let ((char (read-byte *ps-stream*)))
    (unless (= expected char)
      (error "Wrong character. Expected '~C' and got '~C'" (code-char expected) (code-char char)))))

(defun eat-chars (expected)
  (loop for char across expected do (eat-char char)))

(defun skip-white-space-and-comments (&optional eof-error-p)
  (loop for char = (peek-byte *ps-stream* t eof-error-p)
	do (cond ((null char)
		  (return :eof))
		 ((= #!% char)
		  (loop for c = (read-byte *ps-stream* nil)
			until (or (null c)
				  (= #!Newline c) ;; shouldn't happen
				  (= #!Return c)
				  (= #!Linefeed c))
			finally (when (= #!Return c)
				  (alexandria:when-let ((next (peek-byte *ps-stream* nil nil)))
				    (when (= #!Linefeed next)
				      (read-byte *ps-stream*))))))
		 ((not (white-space-p char))
		  (return char))
		 (t
		  (read-byte *ps-stream*)))))

(defun read-object (&optional (eof-error-p t))
  (skip-white-space-and-comments eof-error-p)
  (let ((char (peek-byte *ps-stream* nil eof-error-p)))
    (cond ((null char)
	   :eof)
	  ((= 40 char) ; ( stupid emacs
	   (read-literal-string))
	  ((= #!/ char)
	   (read-name))
	  ((= #!< char)
	   (eat-char #!<)
	   (let ((next (peek-byte *ps-stream*)))
	     (case next
	       (#!< (eat-char #!<) (make-name #"<<" t))
	       (#!~ (read-ascii85-string t))
	       (otherwise
		(read-hexadecimal-string t)))))
	  ((= #!> char)
	   (eat-char #!>)
	   (let ((next (peek-byte *ps-stream*)))
	     (if (eql #!> next)
		 (progn (eat-char #!>)
			(make-name #">>" t))
		 (make-name (serapeum:vect char)))))
	  ((= #!{ char)
	   (read-procedure))
	  ((delimiterp char)
	   (eat-char char)
	   (make-name (serapeum:vect char) t))
	  (t
	   (read-name-or-number)))))

(defun read-literal-string ()
  (eat-char 40)	; ( stupid emacs
  (let ((out (serapeum:vect)))
    (labels ((write-octet (octet)
	       (vector-push-extend octet out)))
      (loop with escape = nil
	    with octal = nil
	    with octal-count = 0
	    with parens = 0
	    for char = (read-byte *ps-stream*)
	    if (= #!\ char)
	      do (cond (escape
			(write-octet 92) ; \ stupid emacs
			(setf escape nil))
		       (t
			(setf escape t)))
	    else if (and octal
			 (digit-octet-p char 8))
		   do (cond ((= 2 octal-count)
			     (write-octet (logand (+ (ash octal 3) (digit-octet-p char 8)) #o377))
			     (setf octal nil
				   octal-count 0))
			    (t
			     (setf octal (+ (ash octal 3) (digit-octet-p char 8)))
			     (incf octal-count)))
	    else do (cond (escape
			   (case char
			     ((#!Newline #!Return #!Linefeed)
			      (when (and (= #!Return char)
					 (eql #!Linefeed (peek-byte *ps-stream* nil nil)))
				(read-byte *ps-stream*)))
			     (#!n
			      (write-octet #!LF))
			     (#!r
			      (write-octet #!CR))
			     (#!t
			      (write-octet #!HT))
			     (#!b
			      (write-octet #!BS))
			     (#!f
			      (write-octet #!FF))
			     (40	; ( stupid emacs
			      (write-octet 40))
			     (41	; ) stupid emacs
			      (write-octet 41))
			     (t
			      (alexandria:if-let ((number (digit-octet-p char 8)))
				(setf octal number
				      octal-count 1)
				(write-octet char))))
			   (setf escape nil))
			  (t
			   (when octal
			     (write-octet (logand octal #o377))
			     (setf octal nil
				   octal-count 0))
			   (case char
			     (40 (incf parens)) ; ( stupid emacs
			     (41 (when (zerop parens) (loop-finish)) (decf parens))) ; ) stupid emacs
			   (write-octet char)))
	    finally (when octal
		      (write-octet (logand octal #o377))))
      (make-instance 'ps-string :value out))))

(defun read-hexadecimal-string (&optional <-eaten)
  (unless <-eaten
    (eat-char #!<))
  (let ((octets (serapeum:vect)))
    (loop with prev = nil
	  for char = (read-byte *ps-stream*)
	  until (= #!> char)
	  unless (white-space-p char)
	    do (cond ((not (digit-octet-p char 16))
		      (error "Wrong thing to be a hexdigit: ~C" (code-char char)))
		     (prev
		      (vector-push-extend (+ (ash prev 4) (digit-octet-p char 16)) octets)
		      (setf prev nil))
		     (t
		      (setf prev (digit-octet-p char 16))))
	  finally (when prev
		    (vector-push-extend (ash prev 4) octets)))
    (make-instance 'ps-string :value octets)))

(defun read-procedure ()
  (eat-char #!{)
  (let ((stack '()))
    (loop (skip-white-space-and-comments)
	  (serapeum:case-let (char (peek-byte *ps-stream*))
	    (#!}
	     (eat-char #!})
	     (return))
	    (t
	     (push (read-object) stack))))
    (make-instance 'ps-procedure :value (nreverse stack))))

(defun read-name ()
  (let ((chars (serapeum:vect))
	(executablep t))
    (let ((first (peek-byte *ps-stream* nil nil :eof)))
      (when (eql first #!/)
	(eat-char #!/)
	(setf executablep nil)))
    (loop for char = (peek-byte *ps-stream* nil nil :eof)
	  until (eq :eof char)
	  while (regular-character-p char)
	  do (vector-push-extend (read-byte *ps-stream*) chars))
    (make-name chars executablep)))

(defun read-name-or-number ()
  (let ((octets (serapeum:vect)))
    (loop for char = (peek-byte *ps-stream* nil nil)
	  while char
	  while (regular-character-p char)
	  do (read-byte *ps-stream*)
	     (vector-push-extend char octets))
    (if (= 1 (count #!# octets))
	(let* ((hash (position #!# octets))
	       (radix-part (subseq octets 0 hash))
	       (num-part (subseq octets (1+ hash))))
	  (when (and radix-part num-part)
	    (multiple-value-bind (radix len)
		(parse-integer (octets-latin1 radix-part) :junk-allowed t)
	      (when (and radix
			 (= len (length radix-part)))
		(multiple-value-bind (num len)
		    (parse-integer (octets-latin1 num-part) :radix radix :junk-allowed t)
		  (when (and num
			   (= len (length num-part)))
		    (return-from read-name-or-number (make-instance 'ps-integer :value num))))))))
	(unless (find #!/ octets)
	  (multiple-value-bind (num len)
	      (read-from-string (octets-latin1 octets) nil nil) ;; FIXME PARSE-NUMBER
	    (when (and num
		       (numberp num)
		       (= len (length octets)))
	      (return-from read-name-or-number (make-instance (if (integerp num) 'ps-integer 'ps-real)
							      :value num))))))
    (with-input-from-octet-vector (*ps-stream* octets)
      (read-name))))

;;;# DECODING STREAM

(defclass decoding-stream (trivial-gray-streams:fundamental-binary-input-stream)
  ((%base-stream
    :initarg :base
    :accessor decoding-stream-base-stream)
   (%peeked-byte
    :accessor peeked-byte
    :initform nil)))

(defgeneric decode-next-byte (stream))
      
(defmethod initialize-instance :after ((stream decoding-stream) &key)
  (setf (peeked-byte stream)
	(decode-next-byte stream)))

(defmethod trivial-gray-streams:stream-read-byte ((stream decoding-stream))
  (unless (open-stream-p stream)
    (error 'stream-error :stream stream))
  (shiftf (peeked-byte stream)
	  (decode-next-byte stream)))

(defmethod trivial-gray-streams:stream-listen ((stream decoding-stream))
  (unless (open-stream-p stream)
    (error 'stream-error :stream stream))
  (not (eq :eof (peeked-byte stream))))

(defmethod trivial-gray-streams:stream-read-sequence ((stream decoding-stream) sequence start end &key)
  (loop for index from start below end
	for char = (read-byte stream nil :eof)
	until (eq :eof char)
	do (setf (elt sequence index) char)
	finally (return index)))

(defmethod trivial-gray-streams:stream-file-position ((stream decoding-stream))
  (file-position (decoding-stream-base-stream stream)))

#+(or) ;; DECODING-STREAMS are stateful, we can't just bounce around them
(defmethod (setf trivial-gray-streams:stream-file-position) (position-spec (stream decoding-stream))
  (unless (integerp position-spec) (error 'stream-error :stream stream))
  (file-position (decoding-stream-base-stream stream) position-spec)
  (setf (peeked-byte stream) (decode-next-byte stream))
  position-spec)

(defmethod peek-byte ((stream decoding-stream) &optional peek-type eof-error-p eof-value)
  (loop for octet = (peeked-byte stream)
	until (cond ((eq :eof octet)
		     (return eof-value))
		    ((null peek-type))
		    ((eq t peek-type)
		     (not (white-space-p octet)))
		    ((= octet peek-type)))
	do (read-byte stream eof-error-p :eof)
	finally (return octet)))

;;;# EEXEC-STREAM

(defclass eexec-stream (decoding-stream)
  ((%r
    :accessor eexec-stream-r
    :initform +eexec-key+)))

(defclass binary-eexec-stream (eexec-stream)
  ())

(defclass asciihex-eexec-stream (eexec-stream)
  ())

(defmethod decode-next-byte ((stream binary-eexec-stream))
  (with-accessors ((R eexec-stream-r) (bs decoding-stream-base-stream)) stream
    (let ((byte (read-byte bs)))
      (if (eq :eof byte)
	  :eof
	  (prog1 (logxor byte (ldb (byte 8 8) R))
	    (setf R (logand (+ +crypt-c2+
			       (* (+ byte R)
				  +crypt-c1+))
			    #xFFFF)))))))

(defmethod decode-next-byte ((stream asciihex-eexec-stream))
  (with-accessors ((R eexec-stream-r) (bs decoding-stream-base-stream)) stream
    (flet ((next-byte () (loop for byte = (read-byte bs)
			       until (eq :eof byte)
			       while (white-space-p byte)
			       finally (return byte))))
      (let ((byte1 (next-byte))
	    (byte2 (next-byte)))
	(if (eq :eof byte1)
	    :eof
	    (let ((byte (if (eq :eof byte2)
			    (ash (digit-octet-p byte1 16) 4) 
			    (+ (ash (digit-octet-p byte1 16) 4)
			       (digit-octet-p byte2 16)))))
	      (prog1 (logxor byte (ldb (byte 8 8) R))
		(setf R (logand (+ +crypt-c2+
				   (* (+ byte R)
				      +crypt-c1+))
				#xFFFF)))))))))

(defgeneric make-eexec-stream (base-stream &optional skip))

(defmethod make-eexec-stream (base-stream &optional (skip 4))
  (let ((fp (file-position base-stream))
	(five (make-array 5 :element-type '(unsigned-byte 8))))
    (read-sequence five base-stream)
    (let ((class (if (every (lambda (byte)
			      (or (white-space-p byte)
				  (digit-octet-p byte 16)))
			    five)
		     'asciihex-eexec-stream
		     'binary-eexec-stream)))
      (file-position base-stream fp)
      (serapeum:lret ((es (make-instance class :base base-stream)))
	(loop repeat skip do (read-byte es))))))

(defconstant +crypt-c1+ 52845)
(defconstant +crypt-c2+ 22719)
(defconstant +eexec-key+ 55665)
(defconstant +charstring-key+ 4330)

;;;# PFB-STREAM

(defconstant +pfb-marker+ 128)
(defconstant +pfb-ascii+ 1)
(defconstant +pfb-binary+ 2)
(defconstant +pfb-done+ 3)

(defclass pfb-stream (decoding-stream)
  ((%block-type
    :accessor block-type
    :initform 0)
   (%block-length
    :initform 0
    :accessor block-length)
   (%bytes-seen
    :accessor bytes-seen
    :initform 0)))

(defmethod decode-next-byte ((stream pfb-stream))
  (cond ((= +pfb-done+ (block-type stream))
	 :eof)
	((= (bytes-seen stream) (block-length stream))
	 (multiple-value-bind (block-type block-length)
	     (pfb-read-block-header (decoding-stream-base-stream stream))
	   (setf (block-type stream) block-type
		 (block-length stream) block-length
		 (bytes-seen stream) 0)
	   (decode-next-byte stream)))
	(t (incf (bytes-seen stream)) 
	   (read-byte (decoding-stream-base-stream stream)))))

(defun pfb-read-block-header (stream)
  (let ((marker (read-byte stream))
	(block-type (read-byte stream))
	(block-length 0))
    (unless (= +pfb-marker+ marker)
      (error "Malformed .PFB file: wanted 128, saw ~D" marker))
    (unless (eq +pfb-done+ block-type)
      (loop repeat 4
	    for shift from 0
	    for byte = (read-byte stream)
	    do (incf block-length (ash byte (* shift 8)))))
    (values block-type block-length)))

(defun make-pfb-stream (base-stream)
  (make-instance 'pfb-stream :base base-stream))

(defmethod make-eexec-stream ((base-stream pfb-stream) &optional (skip 4))
  (read-byte base-stream) ;; FIXME hack to prime the next, presumably binary, block
  (let ((class (cond 
		 ((= +pfb-ascii+ (block-type base-stream))
		  'asciihex-eexec-stream)
		 ((= +pfb-binary+ (block-type base-stream))
		  'binary-eexec-stream)
		 (t (error "Unsuitable .PFB block-type for eexec-stream: ~A"
			   (block-type base-stream))))))
    (serapeum:lret ((es (make-instance class :base base-stream)))
      (loop repeat skip do (read-byte es)))))

;;;# TESTING

(defun decrypt-bytes (bytes key skip)
  (let ((R key)
	(result (serapeum:vect)))
    (loop for cipher across bytes
	  do (vector-push-extend (logxor cipher (ldb (byte 8 8) R))
				 result)
	     (setf R (logand (+ +crypt-c2+
				(* (+ cipher R)
				   +crypt-c1+))
			     #xFFFF)))
    (subseq result skip)))

(defun encrypt-bytes (bytes key skip)
  (let* ((R key)
	 (result (serapeum:vect))
	 (salt (loop repeat skip collect (random 255)))
	 (input (concatenate 'vector salt bytes)))
    (loop for plain across input
	  do (let ((cipher (logxor plain (ldb (byte 8 8) R))))
	       (vector-push-extend cipher result)
	       (setf R (logand (+ +crypt-c2+
				  (* (+ cipher R)
				     +crypt-c1+))
			       #xFFFF))))
    result))
	

(defun decrypt-eexec (bytes)
  (decrypt-bytes bytes +eexec-key+ 4))
 
(defun decrypt-charstring (bytes)
  (decrypt-bytes bytes +charstring-key+ 4))

(defun eexec-test (&optional (count 100))
  (skip-white-space-and-comments nil)
  (loop with bytes = (serapeum:vect)
	repeat count
	for byte = (read-byte *ps-stream* nil :eof)
	until (eq byte :eof)
	do (vector-push-extend byte bytes)
	finally (return (octets-latin1 (decrypt-eexec bytes)))))

(defun extract-eexec (file &optional (count 1000))
  (with-input-from-octet-file (*ps-stream* file)
    (loop with eexec = (make-name #"eexec")
	  with closefile = (make-name #"closefile")
	  for obj = (read-object nil)
	  until (eq :eof obj)
	  when (and (typep obj 'ps-name)
		    (nameql eexec obj))
	    do (read-byte *ps-stream*)
	       (princ (eexec-test count))
	       (loop-finish))))  

(defun skip-to-nd (stream)
  (loop for byte = (read-byte stream nil)
	and prev = nil then byte
	while byte
	until (and prev (= #!N prev) (= #!D byte))
	collecting byte into bytes
	finally (return (decrypt-charstring (apply 'vector (subseq bytes 1 (- (length bytes) 2)))))))

(defun test-charstring (charstring)
  (with-input-from-octet-vector (s charstring)
    (read-type1-charstring s)))

(defun test ()
  (with-input-from-octet-file (*ps-stream* "c:/Users/David/Downloads/urw-base35-fonts-20200910/fonts/StandardSymbolsPS.t1")
    (loop with eexec = (make-name #"eexec")
	  with RD = (make-name #"RD")
	  with closefile = (make-name #"closefile")
	  with glyphs = '()
	  for obj = (read-object nil)
	  until (eq :eof obj)
	  do (print obj)
	  when (and (typep obj 'ps-name)
		    (nameql eexec obj))
	    do (read-byte *ps-stream*)
	       (let ((*ps-stream* (make-eexec-stream *ps-stream*)))		 
		 (loop for obj = (read-object nil)
		       and stack1 = nil then obj
		       until (eq :eof obj)
		       until (and (typep obj 'ps-name)
				  (nameql closefile obj))
		       when (and (typep obj 'ps-name)
				 (nameql RD obj))
			 do (push (test-charstring (skip-to-nd *ps-stream*)) glyphs)
		       else do (print obj)))
	  finally (return glyphs))))

(defun pfb-blocks (file)
  (with-input-from-octet-file (s file)
    (loop (multiple-value-bind (type length)
	      (pfb-read-block-header s)
	    (print type)
	    (when (= +pfb-done+ type)
	      (return))
	    (print length)
	    (let ((stuff (make-array length)))
	      (read-sequence stuff s)
	      (when (= +pfb-ascii+ type)
		(print (octets-latin1 stuff))))))))
