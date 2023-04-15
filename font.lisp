;;;; jqs-type1

(in-package #:com.splittist.type1)

;;;# CFF

(defun read-dict-real (stream)
  (loop with scratch = (make-array 0 :element-type 'character :fill-pointer 0)
	for byte = (read-byte stream)
	for nibble1 = (ldb (byte 4 4) byte)
	for nibble2 = (ldb (byte 4 0) byte)
	do (loop for nibble in (list nibble1 nibble2)
		 do (cond
		      ((<= 0 nibble 9)
		       (vector-push-extend (code-char (+ #.(char-code #\0) nibble))
					   scratch))
		      ((= #xA nibble)
		       (vector-push-extend #\. scratch))
		      ((= #xB nibble)
		       (vector-push-extend #\E scratch))
		      ((= #xC nibble)
		       (vector-push-extend #\E scratch)
		       (vector-push-extend #\- scratch))
		      ((= #xE nibble)
		       (vector-push-extend #\- scratch))
		      ((= #xF nibble)
		       (return-from read-dict-real
			 (read-from-string scratch))))))) ;; FIXME

;; (with-input-from-octet-vector (s #1A(#x8b #xef #x27 #xfa #x7c #xfe #x7c #x1c #x27 #x10 #x1c #xd8 #xf0 #x1d #x00 #x01 #x86 #xa0 #x1d #xff #xfe #x79 #x60 #x1e #xe2 #xa2 #x5f #x1e #x0a #x14 #x05 #x41 #xc3 #xff)) (loop repeat 11 collect (read-dict-object s)))

;; (0 100 -100 1000 -1000 10000 -10000 100000 -100000 -2.25 1.40541e-4)

(defun read-dict-operand (stream b0)
  (cond
    ((<= 32 b0 246)
     (- b0 139))
    ((<= 247 b0 250)
     (let ((b1 (read-byte stream)))
       (+ (* 256 (- b0 247)) b1 108)))
    ((<= 251 b0 254)
     (let ((b1 (read-byte stream)))
       (- (* -256 (- b0 251)) b1 108)))
    ((= 28 b0)
     (let ((result (read-card16 stream)))
       (if (logbitp 15 result)
	   (1- (- (logandc2 #xFFFF result)))
	   result)))
    ((= 29 b0)
       (let ((result (read-card32 stream)))
	 (if (logbitp 31 result)
	     (1- (- (logandc2 #xFFFFFFFF result)))
	     result)))
    ((= 30 b0)
     (read-dict-real stream))))

(defclass cff-font ()
  ((%stream
    :initarg :stream
    :reader cff-font-stream)
   (%off-size
    :initarg :off-size
    :reader cff-font-off-size)
   (%name
    :accessor cff-font-names)
   (%top-dict
    :accessor cff-top-dict)
   (%strings
    :accessor cff-strings)
   (%global-subr
    :accessor cff-global-subrs)
   (%encodings
    :accessor cff-encodings)
   (%charsets
    :accessor cff-charsets)
   (%charstrings
    :accessor cff-charstrings)
   (%privatedicts
    :accessor cff-private-dicts)
   (%local-subrs
    :accessor cff-local-subrs)))

(defun make-cff-font (stream off-size)
  (make-instance 'cff-font :stream stream :off-size off-size))

(defclass cff-index ()
  ((%stream
    :initarg :stream
    :reader cff-index-stream)
   (%count
    :initarg :count
    :reader cff-index-count)
   (%off-size
    :initarg :off-size
    :reader cff-index-off-size)
   (%offset-array
    :initarg :offset-array
    :reader cff-index-offset-array)
   (%data-start
    :initarg :data-start
    :reader cff-index-data-start)))

(defmethod print-object ((object cff-index) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (cff-index-count object) stream)))

(defun make-empty-cff-index (stream)
  (make-instance 'cff-index :count 0 :stream stream))

(defun cff-index-empty-p (index)
  (zerop (cff-index-count index)))

(defun make-cff-index (stream count off-size offset-array data-start)
  (make-instance 'cff-index :stream stream
			    :count count
			    :off-size off-size
			    :offset-array offset-array
			    :data-start data-start))

(defun cff-index-item (index item-number)
  (assert (< item-number (cff-index-count index)))
  (let* ((stream (cff-index-stream index))
	 (offset (aref (cff-index-offset-array index) item-number))
	 (length (- (aref (cff-index-offset-array index) (1+ item-number)) offset)))
    (file-position stream (+ offset (cff-index-data-start index)))
    (read-bytes stream length)))

(defun cff-index-end (index)
  (if (cff-index-empty-p index)
      0
      (aref (cff-index-offset-array index) (cff-index-count index))))
		 
(defun cff-read-index (stream)
  (let ((count (read-card16 stream)))
    (if (zerop count)
	(make-empty-cff-index stream)
	(let* ((off-size (read-off-size stream))
	       (offset-array (read-offset-array stream count off-size))
	       (data-start (1- (file-position stream))))
	  (make-cff-index stream count off-size offset-array data-start)))))

(defun cff-read-header (stream)
  (let ((major (read-card8 stream))
	(minor (read-card8 stream))
	(hdr-size (read-card8 stream))
	(off-size (read-off-size stream)))
    (list major minor hdr-size off-size)))

(defparameter +top-dict-defaults+
  '((:is-fixed-pitch 0)
    (:italic-angle 0)
    (:underline-position -100)
    (:underline-thickness 50)
    (:paint-type 0)
    (:charstring-type 2)
    (:font-matrix #(0.001 0 0 0.001 0))
    (:font-bbox #(0 0 0 0))
    (:stroke-width 0)
    (:charset 0)
    (:encoding 0)))

(defparameter +top-dict-cid-defaults+
  '((:cid-font-version 0)
    (:cid-font-revision 0)
    (:cid-font-type 0)
    (:cid-count 8720)))

(defun make-default-top-dict (&optional cidp)
  (let ((dict (make-hash-table :size 11)))
    (loop for (key value) in +top-dict-defaults+
	  do (setf (gethash key dict) value))
    (when cidp
      (loop for (key value) in +top-dict-cid-defaults+
	    do (setf (gethash key dict) value)))
    dict))

(defun read-top-dict (stream)
  (let ((stack '())
	(dict (make-default-top-dict)))
    (loop for b0 = (read-byte stream nil :eof)
	  until (eq :eof b0)
	  do (case b0
	       (0 (setf (gethash :version dict) (pop stack)))
	       (1 (setf (gethash :notice dict) (pop stack)))
	       (2 (setf (gethash :full-name dict) (pop stack)))
	       (3 (setf (gethash :family-name dict) (pop stack)))
	       (4 (setf (gethash :weight dict) (pop stack)))
	       (5 (setf (gethash :font-bbox dict) (apply 'vector (nreverse stack)))
		(setf stack '()))
	       (12
		(let ((b1 (read-byte stream)))
		  (case b1
		    (0 (setf (gethash :copyright dict) (pop stack)))
		    (1 (setf (gethash :is-fixed-pitch dict) (pop stack)))
		    (2 (setf (gethash :italic-angle dict) (pop stack)))
		    (3 (setf (gethash :underline-position dict) (pop stack)))
		    (4 (setf (gethash :underline-thickness dict) (pop stack)))
		    (5 (setf (gethash :paint-type dict) (pop stack)))
		    (6 (setf (gethash :charstring-type dict) (pop stack)))
		    (7 (setf (gethash :font-matrix dict) (apply 'vector (nreverse stack)))
		     (setf stack '()))
		    (8 (setf (gethash :stroke-width dict) (pop stack)))
		    (20 (setf (gethash :synthetic-base dict) (pop stack)))
		    (21 (setf (gethash :postscript dict) (pop stack)))
		    (22 (setf (gethash :base-font-name dict) (pop stack)))
		    (23 (setf (gethash :base-font-blend dict) (apply 'vector (nreverse stack)))
		     (setf stack '()))
		    ;; CIDFont Operator Extensions
		    (30 (setf (gethash :ros dict) (copy-list stack))
		     (setf stack '()))
		    (31 (setf (gethash :cid-font-version dict) (pop stack)))
		    (32 (setf (gethash :cid-font-revision dict) (pop stack)))
		    (33 (setf (gethash :cid-font-type dict) (pop stack)))
		    (34 (setf (gethash :cid-count dict) (pop stack)))
		    (35 (setf (gethash :uid-base dict) (pop stack)))
		    (36 (setf (gethash :fd-array dict) (pop stack)))
		    (37 (setf (gethash :fd-select dict) (pop stack)))
		    (38 (setf (gethash :font-name dict) (pop stack))))))
	       (13 (setf (gethash :unique-id dict) (pop stack)))
	       (14 (setf (gethash :xuid dict) (apply 'vector (nreverse stack)))
		(setf stack '()))
	       (15 (setf (gethash :charset dict) (pop stack)))
	       (16 (setf (gethash :encoding dict) (pop stack)))
	       (17 (setf (gethash :char-strings dict) (pop stack)))
	       (18 (setf (gethash :private dict) (cons (pop stack) (pop stack))))
	       (otherwise
		(alexandria:when-let ((operand (read-dict-operand stream b0)))
		  (push operand stack)))))
    dict))

(named-readtables:in-readtable syntax)

(defparameter +standard-strings+
  #(#".notdef" #"space" #"exclam" #"quotedbl" #"numbersign" #"dollar"
    #"percent" #"ampersand" #"quoteright" #"parenleft" #"parenright"
    #"asterisk" #"plus" #"comma" #"hyphen" #"period" #"slash" #"zero" #"one"
    #"two" #"three" #"four" #"five" #"six" #"seven" #"eight" #"nine" #"colon"
    #"semicolon" #"less" #"equal" #"greater" #"question" #"at" #"A" #"B" #"C"
    #"D" #"E" #"F" #"G" #"H" #"I" #"J" #"K" #"L" #"M" #"N" #"O" #"P" #"Q" #"R"
    #"S" #"T" #"U" #"V" #"W" #"X" #"Y" #"Z" #"bracketleft" #"backslash"
    #"bracketright" #"asciicircum" #"underscore" #"quoteleft" #"a" #"b" #"c"
    #"d" #"e" #"f" #"g" #"h" #"i" #"j" #"k" #"l" #"m" #"n" #"o" #"p" #"q" #"r"
    #"s" #"t" #"u" #"v" #"w" #"x" #"y" #"z" #"braceleft" #"bar" #"braceright"
    #"asciitilde" #"exclamdown" #"cent" #"sterling" #"fraction" #"yen"
    #"florin" #"section" #"currency" #"quotesingle" #"quotedblleft"
    #"guillemotleft" #"guilsinglleft" #"guilsinglright" #"fi" #"fl" #"endash"
    #"dagger" #"daggerdbl" #"periodcentered" #"paragraph" #"bullet"
    #"quotesinglbase" #"quotedblbase" #"quotedblright" #"guillemotright"
    #"ellipsis" #"perthousand" #"questiondown" #"grave" #"acute" #"circumflex"
    #"tilde" #"macron" #"breve" #"dotaccent" #"dieresis" #"ring" #"cedilla"
    #"hungarumlaut" #"ogonek" #"caron" #"emdash" #"AE" #"ordfeminine" #"Lslash"
    #"Oslash" #"OE" #"ordmasculine" #"ae" #"dotlessi" #"lslash" #"oslash" #"oe"
    #"germandbls" #"onesuperior" #"logicalnot" #"mu" #"trademark" #"Eth"
    #"onehalf" #"plusminus" #"Thorn" #"onequarter" #"divide" #"brokenbar"
    #"degree" #"thorn" #"threequarters" #"twosuperior" #"registered" #"minus"
    #"eth" #"multiply" #"threesuperior" #"copyright" #"Aacute" #"Acircumflex"
    #"Adieresis" #"Agrave" #"Aring" #"Atilde" #"Ccedilla" #"Eacute"
    #"Ecircumflex" #"Edieresis" #"Egrave" #"Iacute" #"Icircumflex" #"Idieresis"
    #"Igrave" #"Ntilde" #"Oacute" #"Ocircumflex" #"Odieresis" #"Ograve"
    #"Otilde" #"Scaron" #"Uacute" #"Ucircumflex" #"Udieresis" #"Ugrave"
    #"Yacute" #"Ydieresis" #"Zcaron" #"aacute" #"acircumflex" #"adieresis"
    #"agrave" #"aring" #"atilde" #"ccedilla" #"eacute" #"ecircumflex"
    #"edieresis" #"egrave" #"iacute" #"icircumflex" #"idieresis" #"igrave"
    #"ntilde" #"oacute" #"ocircumflex" #"odieresis" #"ograve" #"otilde"
    #"scaron" #"uacute" #"ucircumflex" #"udieresis" #"ugrave" #"yacute"
    #"ydieresis" #"zcaron" #"exclamsmall" #"Hungarumlautsmall"
    #"dollaroldstyle" #"dollarsuperior" #"ampersandsmall" #"Acutesmall"
    #"parenleftsuperior" #"parenrightsuperior" #"twodotenleader"
    #"onedotenleader" #"zerooldstyle" #"oneoldstyle" #"twooldstyle"
    #"threeoldstyle" #"fouroldstyle" #"fiveoldstyle" #"sixoldstyle"
    #"sevenoldstyle" #"eightoldstyle" #"nineoldstyle" #"commasuperior"
    #"threequartersemdash" #"periodsuperior" #"questionsmall" #"asuperior"
    #"bsuperior" #"centsuperior" #"dsuperior" #"esuperior" #"isuperior"
    #"lsuperior" #"msuperior" #"nsuperior" #"osuperior" #"rsuperior"
    #"ssuperior" #"tsuperior" #"ff" #"ffi" #"ffl" #"parenleftinferior"
    #"parenrightinferior" #"Circumflexsmall" #"hyphensuperior" #"Gravesmall"
    #"Asmall" #"Bsmall" #"Csmall" #"Dsmall" #"Esmall" #"Fsmall" #"Gsmall"
    #"Hsmall" #"Ismall" #"Jsmall" #"Ksmall" #"Lsmall" #"Msmall" #"Nsmall"
    #"Osmall" #"Psmall" #"Qsmall" #"Rsmall" #"Ssmall" #"Tsmall" #"Usmall"
    #"Vsmall" #"Wsmall" #"Xsmall" #"Ysmall" #"Zsmall" #"colonmonetary"
    #"onefitted" #"rupiah" #"Tildesmall" #"exclamdownsmall" #"centoldstyle"
    #"Lslashsmall" #"Scaronsmall" #"Zcaronsmall" #"Dieresissmall" #"Brevesmall"
    #"Caronsmall" #"Dotaccentsmall" #"Macronsmall" #"figuredash"
    #"hypheninferior" #"Ogoneksmall" #"Ringsmall" #"Cedillasmall"
    #"questiondownsmall" #"oneeighth" #"threeeighths" #"fiveeighths"
    #"seveneighths" #"onethird" #"twothirds" #"zerosuperior" #"foursuperior"
    #"fivesuperior" #"sixsuperior" #"sevensuperior" #"eightsuperior"
    #"ninesuperior" #"zeroinferior" #"oneinferior" #"twoinferior"
    #"threeinferior" #"fourinferior" #"fiveinferior" #"sixinferior"
    #"seveninferior" #"eightinferior" #"nineinferior" #"centinferior"
    #"dollarinferior" #"periodinferior" #"commainferior" #"Agravesmall"
    #"Aacutesmall" #"Acircumflexsmall" #"Atildesmall" #"Adieresissmall"
    #"Aringsmall" #"AEsmall" #"Ccedillasmall" #"Egravesmall" #"Eacutesmall"
    #"Ecircumflexsmall" #"Edieresissmall" #"Igravesmall" #"Iacutesmall"
    #"Icircumflexsmall" #"Idieresissmall" #"Ethsmall" #"Ntildesmall"
    #"Ogravesmall" #"Oacutesmall" #"Ocircumflexsmall" #"Otildesmall"
    #"Odieresissmall" #"OEsmall" #"Oslashsmall" #"Ugravesmall" #"Uacutesmall"
    #"Ucircumflexsmall" #"Udieresissmall" #"Yacutesmall" #"Thornsmall"
    #"Ydieresissmall" #"001.000" #"001.001" #"001.002" #"001.003" #"Black"
    #"Bold" #"Book" #"Light" #"Medium" #"Regular" #"Roman" #"Semibold"))

(defconstant +n-strings+ 391)

(defun sid-string (font sid)
  (if (< sid +n-strings+)
      (aref +standard-strings+ sid)
      (cff-index-item (cff-strings font) (- sid +n-strings+))))

(defun string-sid (font octets)
  (alexandria:if-let ((position (position octets +standard-strings+ :test 'octets=)))
    position
    (error "Unimplemented string search in cff strings")))

(defparameter +standard-encoding+
  #(0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0
    0 0 1 2 3 4 5 6 7 8
    9 10 11 12 13 14 15 16 17 18
    19 20 21 22 23 24 25 26 27 28
    29 30 31 32 33 34 35 36 37 38
    39 40 41 42 43 44 45 46 47 48
    49 50 51 52 53 54 55 56 57 58
    59 60 61 62 63 64 65 66 67 68
    69 70 71 72 73 74 75 76 77 78
    79 80 81 82 83 84 85 86 87 88
    89 90 91 92 93 94 95 0 0 0
    0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0
    0 96 97 98 99 100 101 102 103 104
    105 106 107 108 109 110 0 111 112 113
    114 0 115 116 117 118 119 120 121 122
    0 123 0 124 125 126 127 128 129 130
    131 0 132 133 0 134 135 136 137 0
    0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 138 0 139 0 0
    0 0 140 141 142 143 0 0 0 0
    0 144 0 0 0 145 0 0 146 147
    148 149 0 0 0 0))

(defparameter +expert-encoding+
  #(0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0
    0 0 1 229 230 0 231 232 233 234
    235 236 237 238 13 14 15 99 239 240
    241 242 243 244 245 246 247 248 27 28
    249 250 251 252 0 253 254 255 256 257
    0 0 0 258 0 0 259 260 261 262
    0 0 263 264 265 0 266 109 110 267
    268 269 0 270 271 272 273 274 275 276
    277 278 279 280 281 282 283 284 285 286
    287 288 289 290 291 292 293 294 295 296
    297 298 299 300 301 302 303 0 0 0
    0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0
    0 0 0 0 0 0 0 0 0 0
    0 304 305 306 0 0 307 308 309 310
    311 0 312 0 0 313 0 0 314 315
    0 0 316 317 318 0 0 0 158 155
    163 319 320 321 322 323 324 325 0 0
    326 150 164 169 327 328 329 330 331 332
    333 334 335 336 337 338 339 340 341 342
    343 344 345 346 347 348 349 350 351 352
    353 354 355 356 357 358 359 360 361 362
    363 364 365 366 367 368 369 370 371 372
    373 374 375 376 377 378))

(defun read-encoding (stream offset)
  (cond ((= 0 offset)
	 (list +standard-encoding+))
	((= 1 offset)
	 (list +expert-encoding+))
	(t
	 (let* ((encoding (make-array 256 :initial-element 0)) ; element type?
		(supplement nil)
		(raw-format (read-card8 stream))
		(supplementp (logbitp 7 raw-format))
		(format (logand 127 raw-format)))
	   (cond ((= 0 format)
		  (let ((ncodes (read-card8 stream)))
		    (loop repeat ncodes
			  for index from 0
			  do (setf (aref encoding index) (read-card8 stream)))))
		 ((= 1 format)
		  (let ((nranges (read-card8 stream)))
		    (loop with index = 0
			  repeat nranges
			  for first = (read-card8 stream)
			  for nleft = (read-card8 stream)
			  do (setf (aref encoding index) first)
			     (incf index)
			     (loop repeat nleft
				   for code from (1+ first)
				   do (setf (aref encoding index) code)
				      (incf index)))))
		 (t
		  (error "Unknown encoding format: ~D" format)))
	   (when supplementp
	     (let ((nsups (read-card8 stream)))
	       (loop repeat nsups
		     for code = (read-card8 stream)
		     for glyph = (read-sid stream)
		     do (push (cons code glyph) supplement))))
	   (list encoding supplement)))))

(defparameter +iso-adobe-charset+
  #(0 1 2 3 4 5 6 7 8 9
    10 11 12 13 14 15 16 17 18 19
    20 21 22 23 24 25 26 27 28 29
    30 31 32 33 34 35 36 37 38 39
    40 41 42 43 44 45 46 47 48 49
    50 51 52 53 54 55 56 57 58 59
    60 61 62 63 64 65 66 67 68 69
    70 71 72 73 74 75 76 77 78 79
    80 81 82 83 84 85 86 87 88 89
    90 91 92 93 94 95 96 97 98 99
    100 101 102 103 104 105 106 107 108 109
    110 111 112 113 114 115 116 117 118 119
    120 121 122 123 124 125 126 127 128 129
    130 131 132 133 134 135 136 137 138 139
    140 141 142 143 144 145 146 147 148 149
    150 151 152 153 154 155 156 157 158 159
    160 161 162 163 164 165 166 167 168 169
    170 171 172 173 174 175 176 177 178 179
    180 181 182 183 184 185 186 187 188 189
    190 191 192 193 194 195 196 197 198 199
    200 201 202 203 204 205 206 207 208 209
    210 211 212 213 214 215 216 217 218 219
    220 221 222 223 224 225 226 227 228))

(defparameter +expert-charset+
  #(0 1 229 230 231 232 233 234 235 236
    237 238 13 14 15 99 239 240 241 242
    243 244 245 246 247 248 27 28 249 250
    251 252 253 254 255 256 257 258 259 260
    261 262 263 264 265 266 109 110 267 268
    269 270 271 272 273 274 275 276 277 278
    279 280 281 282 283 284 285 286 287 288
    289 290 291 292 293 294 295 296 297 298
    299 300 301 302 303 304 305 306 307 308
    309 310 311 312 313 314 315 316 317 318
    158 155 163 319 320 321 322 323 324 325
    326 150 164 169 327 328 329 330 331 332
    333 334 335 336 337 338 339 340 341 342
    343 344 345 346 347 348 349 350 351 352
    353 354 355 356 357 358 359 360 361 362
    363 364 365 366 367 368 369 370 371 372
    373 374 375 376 377 378))

(defparameter +expert-subset-charset+
  #(0 1 231 232 235 236 237 238 13 14
    15 99 239 240 241 242 243 244 245 246
    247 248 27 28 249 250 251 253 254 255
    256 257 258 259 260 261 262 263 264 265
    266 109 110 267 268 269 270 272 300 301
    302 305 314 315 158 155 163 320 321 322
    323 324 325 326 150 164 169 327 328 329
    330 331 332 333 334 335 336 337 338 339
    340 341 342 343 344 345 346))

(defun read-charset (stream offset glyph-count)
  (cond ((= 0 offset)
	 +iso-adobe-charset+)
	((= 1 offset)
	 +expert-charset+)
	((= 2 offset)
	 +expert-subset-charset+)
	(t
	 (file-position stream offset)
	 (let ((charset (make-array (1- glyph-count)))
	       (format (read-card8 stream)))
	   (cond ((= 0 format)
		  (loop for index below glyph-count
			do (setf (aref charset index) (read-sid stream))))
		 ((or (= 1 format)
		      (= 2 format))
		  (let ((read-nleft (if (= 1 format) #'read-card8 #'read-card16)))
		    (loop with index = 0
			  for first = (read-sid stream)
			  for nleft = (funcall read-nleft stream)
			  until (= index (1- glyph-count))
			  do (setf (aref charset index) first)
			     (incf index)
			     (loop repeat nleft
				   for code from (1+ first)
				   do (setf (aref charset index) code)
				      (incf index)))))
		 (t
		  (error "Unknown charset format: ~D" format)))
	   charset))))

(defparameter +private-dict-defaults+
  '((:blue-scale 0.039625)
    (:blue-shift 7)
    (:blue-fuzz 1)
    (:force-bold 0) ;; false
    (:language-group 0)
    (:expansion-factor 0.06)
    (:initial-random-seed 0)
    (:default-width-x 0)
    (:default-width-y 0)))

(defun make-default-private-dict ()
  (let ((dict (make-hash-table :size 9)))
    (loop for (key value) in +private-dict-defaults+
	  do (setf (gethash key dict) value))
    dict))

(defun read-private-dict (stream)
  (let ((stack '())
	(dict (make-default-private-dict)))
    (loop for b0 = (read-byte stream nil :eof)
	  until (eq :eof b0)
	  do (case b0
	       (6 (setf (gethash :blue-values dict) (apply 'vector (nreverse stack))) ;; delta
		(setf stack '()))
	       (7 (setf (gethash :other-blues dict) (apply 'vector (nreverse stack))) ;; delta
		(setf stack '()))
	       (8 (setf (gethash :family-blues dict) (apply 'vector (nreverse stack))) ;; delta
		(setf stack '()))
	       (9 (setf (gethash :family-other-blues dict) (apply 'vector (nreverse stack))) ;; delta
		(setf stack '()))
	       (10 (setf (gethash :std-h-w dict) (pop stack))) ;; number
	       (11 (setf (gethash :std-v-w dict) (pop stack))) ;; number
	       (12
		(let ((b1 (read-byte stream)))
		  (case b1
		    (9 (setf (gethash :blue-scale dict) (pop stack))) ;;  number
		    (10 (setf (gethash :blue-shift dict) (pop stack))) ;; number
		    (11 (setf (gethash :blue-fuzz dict) (pop stack))) ;; number
		    (12 (setf (gethash :stem-snap-h dict) (apply 'vector (nreverse stack))) ;; delta
		     (setf stack '()))
		    (13 (setf (gethash :stem-snap-v dict) (apply 'vector (nreverse stack))) ;;  delta
		     (setf stack '()))
		    (14 (setf (gethash :force-bold dict) (pop stack))) ;; boolean
		    (17 (setf (gethash :language-group dict) (pop stack))) ;; number
		    (18 (setf (gethash :expansion-factor dict) (pop stack))) ;; number
		    (19 (setf (gethash :initial-random-seed dict) (pop stack)))))) ;; number
	       (19 (setf (gethash :subrs dict) (pop stack))) ;; offset
	       (20 (setf (gethash :default-width-x dict) (pop stack))) ;; number
	       (21 (setf (gethash :nominal-width-x dict) (pop stack))) ;; number
	       (otherwise
		(alexandria:when-let ((operand (read-dict-operand stream b0)))
		  (push operand stack)))))
    dict))

(defun read-cff (stream)
  (destructuring-bind (major minor hdr-size off-size)
      (cff-read-header stream)
    (unless (= 1 major)
      (error "Unknown CFF format: ~D" major))
    (unless (= 0 minor)
      (warn "Unsupported CFF minor format: ~D" minor))
    (let ((font (make-cff-font stream off-size))
	  (top-dict-index nil))
      (file-position stream hdr-size)
      (setf (cff-font-names font) (cff-read-index stream))
      (skip-bytes stream (1- (cff-index-end (cff-font-names font))))
      (setf top-dict-index (cff-read-index stream))
      (skip-bytes stream (1- (cff-index-end top-dict-index)))
      (setf (cff-strings font) (cff-read-index stream))
      (skip-bytes stream (1- (cff-index-end (cff-strings font))))
      (setf (cff-global-subrs font) (cff-read-index stream))
      (setf (cff-top-dict font) ;; pretend there's 1 for now 
	    (read-top-dict (make-octet-vector-stream (cff-index-item top-dict-index 0))))
      (setf (cff-encodings font) ;; pretend there's 1 for now
	    (read-encoding stream (gethash :encoding (cff-top-dict font))))
      (file-position stream (gethash :char-strings (cff-top-dict font)))
      (setf (cff-charstrings font) (cff-read-index stream))
      (setf (cff-charsets font) ;; pretend there's 1 for now
	    (read-charset stream
			  (gethash :charset (cff-top-dict font))
			  (cff-index-count (cff-charstrings font))))
      (file-position stream (car (gethash :private (cff-top-dict font))))
      (setf (cff-private-dicts font) ;; pretend there's 1 for now
	    (read-private-dict stream))
      (alexandria:when-let ((subrs-offset (gethash :subrs (cff-private-dicts font))))
	(file-position stream (+ (car (gethash :private (cff-top-dict font)))
				 subrs-offset))
	(setf (cff-local-subrs font) (cff-read-index stream)))
    font)))

(defun glyph-count (font)
  (cff-index-count (cff-charstrings font)))

(defun italic-angle (font)
  (gethash :italic-angle (cff-top-dict font)))

(defun underline-thickness (font)
  (gethash :underline-thickness (cff-top-dict font)))

(defun underline-position (font)
  (gethash :underline-position (cff-top-dict font)))

(defun fixed-pitch-p (font)
  (= 1 (gethash :is-fixed-pitch (cff-top-dict font))))

(defun full-name (font)
  (alexandria:when-let ((sid (gethash :full-name (cff-top-dict font))))
    (sid-string font sid)))

(defun family-name (font)
  (alexandria:when-let ((sid (gethash :family-name (cff-top-dict font))))
    (sid-string font sid)))

(defun weight (font)
  (alexandria:when-let ((sid (gethash :weight (cff-top-dict font))))
    (sid-string font sid)))

(defun font-matrix (font)
  (gethash :font-matrix (cff-top-dict font)))

(defgeneric bounding-box (thing)
  (:method ((font cff-font))
    (gethash :font-bbox (cff-top-dict font))))

(defun character-index-glyph (font character-index)
  (cff-index-item (cff-charstrings font) character-index))

(defun character-code-name (font character-code)
  (sid-string font (aref (cff-charsets font) (1- character-code))))

(defun charstring-type (font)
  (gethash :charstring-type (cff-top-dict font)))

(defun nominal-width-x (font)
  (gethash :nominal-width-x (cff-private-dicts font)))

(defun default-width-x (font)
  (gethash :default-widht-x (cff-private-dicts font)))

;;;# OFF

