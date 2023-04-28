;;;; jqs-type1 FIXME - ABSTRACT FROM PDFREADER

(in-package #:com.splittist.type1)

;;;# OCTETS

(deftype octet ()
  '(unsigned-byte 8))

(deftype octet-vector (&optional length)
  `(array octet (,length)))

(defun octets-latin1 (octets)
  (map 'string #'code-char octets))

(defun latin1-octets (string)
  (map '(vector octet) #'char-code string))

(defun digit-octet-p (octet &optional (radix 10)) ;; FIXME?
  (digit-char-p (code-char octet) radix))

(defun octets= (vector1 vector2)
  (not (mismatch vector1 vector2)))

;;;# CFF

(defun read-card8 (stream)
  (read-byte stream))

(defun read-card16 (stream)
  (loop repeat 2
	for value = (read-byte stream)
	  then (logior (ash value 8) (read-byte stream))
	finally (return value)))

(defun read-card32 (stream)
  (loop repeat 4
	for value = (read-byte stream)
	  then (logior (ash value 8) (read-byte stream))
	finally (return value)))

(defun read-offset (off-size stream)
  (loop repeat off-size
	for value = (read-byte stream)
	  then (logior (ash value 8) (read-byte stream))
	finally (return value)))

(defun read-off-size (stream)
  (read-byte stream))

(defun read-offset-array (stream count off-size)
  (let ((array (make-array (1+ count) :element-type `(unsigned-byte ,(* 8 off-size)))))
    (dotimes (index (1+ count) array)
      (setf (aref array index) (read-offset off-size stream)))))

(defun read-sid (stream)
  (let ((int (read-card16 stream)))
    (assert (<= 0 int 64999))
    int))

(defun read-16.16 (stream)
  (/ (read-card32 stream) 65536))

(defun read-bytes (stream count) ;; FIXME devel
  (let* ((result (make-array count :element-type 'octet))
	 (bytes-read (read-sequence result stream)))
    (unless (= count bytes-read)
      (error "End of file in READ-BYTES"))
    result))

(defun skip-bytes (stream count)
  (file-position stream (+ (file-position stream) count)))

(defun seek-to-offset (stream offset &optional (base 0))
  (file-position stream (+ offset base)))

;;;# TYPE 1

(defun read-int32 (stream)
  (let ((u32 (read-card32 stream)))
    (if (logbitp 31 u32)
	(1- (- (logandc2 #xFFFFFFFF u32)))
	u32)))

;;;# OCTET-VECTOR-STREAM

(defclass octet-vector-stream (trivial-gray-streams:fundamental-binary-input-stream)
  ((%vector
    :initarg :vector
    :accessor vector-stream-vector)
   (%index
    :initarg :index
    :accessor vector-stream-index
    :initform 0)
   (%end
    :initarg :end
    :accessor vector-stream-end)))

(defgeneric peek-byte (stream &optional peek-type eof-error-p eof-value)
  (:method ((stream octet-vector-stream) &optional peek-type (eof-error-p t) eof-value)
    (let ((index (vector-stream-index stream)))
      (loop for octet = (read-byte stream eof-error-p :eof)
	    for new-index from index
	    until (cond ((eq :eof octet)
			 (return eof-value))
			((null peek-type))
			((eq t peek-type)
			 (not (white-space-p octet)))
			((= octet peek-type)))
	    finally (setf (vector-stream-index stream) new-index)
		    (return octet)))))

(defmethod trivial-gray-streams:stream-read-byte ((stream octet-vector-stream))
  (unless (open-stream-p stream)
    (error 'stream-error :stream stream))
  (with-accessors ((vector vector-stream-vector)
		   (index vector-stream-index)
		   (end vector-stream-end))
      stream
    (if (< index end)
	(prog1 (aref vector index)
	  (incf index))
	:eof)))

(defmethod trivial-gray-streams:stream-listen ((stream octet-vector-stream))
  (unless (open-stream-p stream)
    (error 'stream-error :stream stream))
  (with-accessors ((index vector-stream-index)
		   (end Vector-stream-end))
      stream
    (< index end)))

(defmethod trivial-gray-streams:stream-read-sequence ((stream octet-vector-stream) sequence start end &key)
  (loop with vector-end = (vector-stream-end stream)
	with vector = (vector-stream-vector stream)
	for index from start below end
	for vector-index = (vector-stream-index stream)
	while (< vector-index vector-end)
	do (let ((elt (aref vector vector-index)))
	     (setf (elt sequence index) elt))
	   (incf (vector-stream-index stream))
	finally (return index)))

(defmethod trivial-gray-streams:stream-file-position ((stream octet-vector-stream))
  (vector-stream-index stream))

(defmethod (setf trivial-gray-streams:stream-file-position) (position-spec (stream octet-vector-stream))
  (setf (vector-stream-index stream)
	(case position-spec
	  (:start 0)
	  (:end (vector-stream-end stream))
	  (otherwise
	   (unless (integerp position-spec)
	     (error 'stream-error :stream stream)) ; FIXME better condition
	   (unless (<= 0 position-spec (vector-stream-end stream))
	     (error 'stream-error :stream stream)) ; FIXME better condition
	   position-spec)))
  position-spec)

(defun make-octet-vector-stream (vector &key (start 0) (end (length vector)))
  ;;(check-type vector (vector octet)) ;; DEBUG FIXME
  (make-instance 'octet-vector-stream
		 :vector vector
		 :index start
		 :end end))

(defmacro with-input-from-octet-vector ((var vector &key start end) &body body)
  (alexandria:once-only (vector)
    `(let (,var)
       (unwind-protect
	    (progn
	      (setf ,var (make-octet-vector-stream ,vector
						   :start (or ,start 0)
						   :end (or ,end (length ,vector))))
	      ,@body)
	 (when ,var (close ,var))))))

;;;# OCTET-FILE-STREAM

(defclass octet-file-stream (trivial-gray-streams:fundamental-binary-input-stream)
  ((%file-stream
    :initarg :file-stream
    :accessor octet-file-stream-file-stream)
   (%peeked-byte
    :accessor peeked-byte
    :initform nil)))

(defmethod initialize-instance :after ((stream octet-file-stream) &key)
  (setf (peeked-byte stream)
	(read-byte (octet-file-stream-file-stream stream) nil :eof)))

(defmethod trivial-gray-streams:stream-read-byte ((stream octet-file-stream))
  (unless (open-stream-p stream)
    (error 'stream-error :stream stream))
  (shiftf (peeked-byte stream)
	  (read-byte (octet-file-stream-file-stream stream) nil :eof)))

(defmethod trivial-gray-streams:stream-listen ((stream octet-file-stream))
  (unless (open-stream-p stream)
    (error 'stream-error :stream stream))
  (not (eq :eof (peeked-byte stream))))

(defmethod trivial-gray-streams:stream-read-sequence ((stream octet-file-stream) sequence start end &key)
  (loop for index from start below end
	for char = (read-byte stream nil :eof)
	until (eq :eof char)
	do (setf (elt sequence index) char)
	finally (return index)))

(defmethod trivial-gray-streams:stream-file-position ((stream octet-file-stream))
  (file-position (octet-file-stream-file-stream stream)))

(defmethod (setf trivial-gray-streams:stream-file-position) (position-spec (stream octet-file-stream))
  (unless (integerp position-spec) (error 'stream-error :stream stream))
  (file-position (octet-file-stream-file-stream stream) position-spec)
  (setf (peeked-byte stream) (read-byte (octet-file-stream-file-stream stream)))
  position-spec)

(defmethod peek-byte ((stream octet-file-stream) &optional peek-type eof-error-p eof-value)
  (loop for octet = (peeked-byte stream)
	until (cond ((eq :eof octet)
		     (return eof-value))
		    ((null peek-type))
		    ((eq t peek-type)
		     (not (white-space-p octet)))
		    ((= octet peek-type)))
	do (read-byte stream eof-error-p :eof)
	finally (return octet)))

(defun make-octet-file-stream (filespec)
  (make-instance 'octet-file-stream
		 :file-stream (open filespec :element-type 'octet)))

(defmacro with-input-from-octet-file ((var filespec) &body body)
  (alexandria:with-gensyms (file-stream)
    `(let (,var ,file-stream)
       (unwind-protect
	(progn
	  (setf ,file-stream (open ,filespec :element-type 'octet)
		,var (make-instance 'octet-file-stream :file-stream ,file-stream))
	  ,@body)
	 (when ,file-stream (close ,file-stream))
	 (when ,var (close ,var))))))

;;;# STREAM-LENGTH

(defun stream-length (stream)
  (etypecase stream
    ((or file-stream synonym-stream broadcast-stream)
     (file-length stream))
    (octet-vector-stream
     (vector-stream-end stream))
    (octet-file-stream
     (+ (file-length (octet-file-stream-file-stream stream))
	(if (eq :eof (peeked-byte stream)) 0 1)))))
