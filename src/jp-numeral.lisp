;;; -*- coding: utf-8; -*-

(in-package :jp-numeral)

;;; Conditions

(define-condition no-power-char-error (error)
  ())

(define-condition not-formattable-error (error)
  ())


;;; Accessors to the jp-numeral-table.

(defun style-to-index (style)
  (ecase style
    (:normal +TABLE-NORMAL-INDEX+)
    (:formal +TABLE-FORMAL-INDEX+)
    (:old +TABLE-OLD-INDEX+)
    (:positional +TABLE-POSITIONAL-INDEX+)))

(defun get-digit (n style)
  (assert (<= 0 n 9))
  (let ((entry (aref +digits+ n)))
    (aref entry (style-to-index style))))

(defun get-power (n style)
  (alexandria:if-let ((entry (gethash n +power-hash-table+)))
    (aref entry (style-to-index style))
    (error 'no-power-char-error)))

(defun get-minus-sign (style)
  (aref +minus-sign+ (style-to-index style)))

(defun get-parts-of (style)
  (aref +fraction-parts-of+ (style-to-index style)))

(defun get-radix-point (style)
  (aref +radix-point+ (style-to-index style)))

(defun get-yen (style)
  (aref +yen+ (style-to-index style)))

(defun get-sen (style)
  (aref +sen+ (style-to-index style)))

(defun get-wari (style)
  (aref +wari+ (style-to-index style)))


;;; Writers

(defgeneric write-jp-numeral
    (stream object style
	    &key digits-after-dot scale radix-point-string
	    radix-point-required-p)
  (:method (stream object style &key &allow-other-keys)
    (declare (ignore stream object style))
    (error 'not-formattable-error)))


(defmethod write-jp-numeral :around (stream (object rational) style
				     &rest args
				     &key scale &allow-other-keys)
  (let* ((scaled-object (* object (expt 10 scale))))
    ;; If they are not same type, dispatch the object again.
    (apply (if (alexandria:type= (type-of object)
				 (type-of scaled-object))
	       #'call-next-method
	       #'write-jp-numeral)
	   stream scaled-object style
	   :scale 0
	   args)))

(defun translate-digit-char (c style)
  (ecase c
    (#\0 (get-digit 0 style))
    (#\1 (get-digit 1 style))
    (#\2 (get-digit 2 style))
    (#\3 (get-digit 3 style))
    (#\4 (get-digit 4 style))
    (#\5 (get-digit 5 style))
    (#\6 (get-digit 6 style))
    (#\7 (get-digit 7 style))
    (#\8 (get-digit 8 style))
    (#\9 (get-digit 9 style))))

;; positional notation

(defun write-positional-from-string (stream lispstr style
				     radix-point-string)
  (loop for c across lispstr
     as jp-str =
       (case c
	 ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
	  (translate-digit-char c style))
	 (#\. radix-point-string)
	 (#\- (get-minus-sign style))
	 (#\/ (get-parts-of style))
	 (#\Space "")
	 (otherwise (error 'not-formattable-error)))
     do (write-string jp-str stream)))

(defmethod write-jp-numeral (stream (object integer) (style (eql :positional))
			     &key digits-after-dot scale radix-point-string
			     radix-point-required-p)
  (declare (ignore digits-after-dot)
	   (ignorable scale))
  (assert (zerop scale))
  (write-positional-from-string
   stream
   (format nil "~D~@[.~]" object radix-point-required-p)
   style radix-point-string))

(defmethod write-jp-numeral (stream (object ratio) (style (eql :positional))
			     &key digits-after-dot scale radix-point-string
			     radix-point-required-p)
  (declare (ignore digits-after-dot)
	   (ignorable scale))
  (assert (zerop scale))
  (write-positional-from-string
   stream
   (format nil "~D/~D~@[.~]" (numerator object) (denominator object) radix-point-required-p)
   style radix-point-string))


(defun float-sufficient-width (flt)
  (let ((from-pow (if (zerop flt)
		      1
		      (1+ (abs (log flt 10)))))
	(from-mantissa (* (float-precision flt)
			  (log (float-radix flt) 10)))
	(reserved 2)) 			; dot, 0
    (+ (ceiling from-pow)
       (ceiling from-mantissa)
       reserved)))

(defun stringify-float (flt digits-after-dot scale)
  ;; width is required for working 'scale'.
  ;; (If both width and digits-after-dot are nil, it does not work..)
  (let* ((width (+ (float-sufficient-width flt)
		   (if digits-after-dot
		       (abs digits-after-dot) ; may be minus..
		       0)))
	 (ret (format nil "~v,v,vF" width digits-after-dot scale flt)))
    (when (or (null digits-after-dot)
	      (minusp digits-after-dot))
      (setf ret (string-right-trim '(#\0) ret)))
    ret))

(defmethod write-jp-numeral (stream (object float) (style (eql :positional))
			     &key digits-after-dot scale radix-point-string
			     radix-point-required-p)
  (declare (ignore radix-point-required-p)) ; radix point is always put.
  (write-positional-from-string
   stream
   (stringify-float object digits-after-dot scale)
   style radix-point-string))

;; normal notation

(defun make-digits4-string (digits4 style base-power)
  (declare (type (integer 0 9999) digits4))
  (with-output-to-string (stream)
    (labels ((fill-1-p (style)
	       (ecase style
		 (:normal nil)
		 ((:formal :old) t)))
	     (put-digit (digit pow rest)
	       (when (or (>= digit 2)
			 (and (= digit 1)
			      (ecase pow
				(3 (or (fill-1-p style)
				       (and (zerop rest)
					    (plusp base-power))))
				(2 (fill-1-p style))
				(1 nil)
				(0 t))))
		 (write-string (get-digit digit style) stream))
	       (when (>= digit 1)
		 (write-string (get-power pow style) stream))))
      (multiple-value-bind (d3 d3-rest) (floor digits4 1000)
	(put-digit d3 3 d3-rest)
	(multiple-value-bind (d2 d2-rest) (floor d3-rest 100)
	  (put-digit d2 2 d2-rest)
	  (multiple-value-bind (d1 d0) (floor d2-rest 10)
	    (put-digit d1 1 d0)
	    (put-digit d0 0 0)))))))

(defun print-jp-plus-integer (stream object style)
  (declare (type integer object))
  (loop with strs = nil
     for power from 0 by 4
     for (rest digits4) = (multiple-value-list (floor object 10000))
     then (multiple-value-list (floor rest 10000))
     as digits4-str = (make-digits4-string digits4 style power)
     when (plusp (length digits4-str))
     do (push (get-power power style) strs)
       (push digits4-str strs)
     while (plusp rest)
     finally (mapc #'(lambda (s) (write-string s stream)) strs)))

(defmethod write-jp-numeral (stream (object integer) style
			     &key digits-after-dot scale radix-point-string
			     radix-point-required-p)
  (declare (ignore digits-after-dot)
	   (ignorable scale))
  (assert (zerop scale))
  (when (minusp object)
    (write-string (get-minus-sign style) stream))
  (if (zerop object)
      (write-string (get-digit 0 style) stream)
      (print-jp-plus-integer stream (abs object) style))
  (when radix-point-required-p
    (write-string radix-point-string stream)))

(defmethod write-jp-numeral (stream (object ratio) style
			     &key digits-after-dot scale radix-point-string
			     radix-point-required-p)
  (declare (ignore digits-after-dot)
	   (ignorable scale))
  (assert (zerop scale))
  (when (minusp object)
    (write-string (get-minus-sign style) stream))
  (print-jp-plus-integer stream (denominator object) style)
  (write-string (get-parts-of style) stream)
  (print-jp-plus-integer stream (abs (numerator object)) style)
  (when radix-point-required-p
    (write-string radix-point-string stream)))


(defmethod write-jp-numeral (stream (object float) style
			     &key digits-after-dot scale radix-point-string
			     radix-point-required-p)
  (when (minusp object)
    (write-string (get-minus-sign style) stream)
    (setf object (- object)))
  (let* ((lispstr (stringify-float object digits-after-dot scale))
	 (dot-pos (alexandria:if-let ((pos (position #\. lispstr)))
		    pos
		    (error 'not-formattable-error)))
	 (int-part (parse-integer lispstr :end dot-pos))
	 (frac-part-as-int (or (parse-integer lispstr :start (1+ dot-pos)
					      :junk-allowed t)
			       0)))
    ;; int part
    (print-jp-plus-integer stream int-part style)
    ;; prints '0' if needed
    (when (and (zerop int-part)
	       (or radix-point-required-p
		   (zerop frac-part-as-int)))
      (write-string (get-digit 0 style) stream))
    ;; prints '.' if needed
    (when (or radix-point-required-p
	      (and (not (zerop int-part))
		   (plusp frac-part-as-int)))
      (write-string radix-point-string stream))
    ;; frac part
    (loop for i from (1+ dot-pos) below (length lispstr)
       as c = (aref lispstr i)
       for power downfrom -1
       unless (digit-char-p c)
       do (error 'not-formattable-error)
       if (char/= #\0 c)
       do (write-string (translate-digit-char c style) stream)
       (write-string (get-power power style) stream))))


;; entry point
(defun format-jp-numeral (stream object style
			  &key digits-after-dot scale radix-point)
  "This function works same as `jp', but arranges the arguments for
convenience of calling from ordinary functions.
This writes OBJECT into STREAM as Japanese numerals with the style
specified by STYLE.

Arguments are same as `jp', except below:

- STYLE

  Specify the style of output. Pass one of these symbols:

  - `:normal'	:: Use normal Japanese numerals.
  - `:formal'	:: Use formal styles.
  - `:old'	:: Use old glyphs.
  - `:positional' :: Use positional notations.

====

`jp' と同じことを行うが、普通の関数として呼び出すのに便利なように
引数を置きかえている。
STYLE で指定した形式で、 STREAM に OBJECT を書き出す。

以下の引数以外は、`jp' の引数と同様。

- STYLE

  出力形式を指定する。以下のいずれかのシンボルを渡す。

  - `:normal'	:: 一般的な漢数字を使用する。
  - `:formal'	:: 大字を使用する。
  - `:old'	:: 旧字体を使用する。
  - `:positional' :: 位取り記数法を使用する。"
  (check-type object number "jp-numeral acceptable type")
  (prog ((*print-base* 10)    ; *print-base* must be 10 for jp-numeral
	 (scale (or scale 0))
	 (radix-point-str (etypecase radix-point
			    (string radix-point)
			    (character (string radix-point))
			    (null (get-radix-point style))))
	 (buf (make-array '(1) :element-type 'character :fill-pointer 0 :adjustable t)))
   try-again
   (handler-case
       (with-output-to-string (buf-stream buf)
	 (write-jp-numeral buf-stream object style
			   :digits-after-dot digits-after-dot
			   :scale scale
			   :radix-point-string radix-point-str
			   :radix-point-required-p (if radix-point t nil)))
     (no-power-char-error ()
       ;; Decimal power chars are exhausted. Use positional.
       (assert (not (eq style :positional)))
       (setf style :positional)
       (setf (fill-pointer buf) 0)
       (go try-again))
     (not-formattable-error ()
       ;; complex, Infinity or NaN.
       (setf style nil)
       (setf (fill-pointer buf) 0)
       (format buf "~A" object)))
   ;; Final output.
   (write-string buf stream))
  style) ; If this is not expected one by the caller, means alternative methods used.


;;; cl:format interface

(defun flag-to-style (colon-p at-sign-p)
  (cond ((and colon-p at-sign-p) :positional)
	(colon-p :formal)
	(at-sign-p :old)
	(t :normal)))

(defun jp (stream object &optional colon-p at-sign-p
	   digits-after-dot scale radix-point)
  "This function writes OBJECT into STREAM as Japanese numerals.
This can be called from `cl:format' with '~/' directive.

Arguments are:

- STREAM

  Output destination stream.

- OBJECT

  The object to be outputted.

- COLON-P, AT-SIGN-P

  COLON-P and AT-SIGN-P specify the style for printing.
  The corresponding is below:

  - (and (not colon-p) (not at-sign-p))	:: Use normal Japanese numerals.
  - (and colon-p (not at-sign-p))	:: Use formal styles.
  - (and (not colon-p) at-sign-p)	:: Use Old glyphs.
  - (and colon-p at-sign-p)	:: Use positional Notation.

- DIGITS-AFTER-DOT

  Specifies how many digits putted after the radix point when
  printing a floating-point number.
  When `nil' is specified, uses an appropriate one.

- SCALE

  When printing, uses a number mutiplied with (expt 10 scale).

- RADIX-POINT

  Specifies a character or a string used as a radix point.

====

STREAM に OBJECT を漢数字として書き出す。
`cl:format' の `~/` での関数呼びだしでも使用できる。

引数:

- STREAM

  出力先の stream
	
- OBJECT

  出力する object
	
- COLON-P, AT-SIGN-P
	
  二つの組み合わせで出力形式を指定する。対応は以下の通り:

  - (and (not colon-p) (not at-sign-p))	:: 通常の漢数字
  - (and colon-p (not at-sign-p))	:: 大字
  - (and (not colon-p) at-sign-p)	:: 旧字体
  - (and colon-p at-sign-p) :: 位取り記数法

- DIGITS-AFTER-DOT

  浮動小数点数を出力する時に、小数点の後に何桁目まで出力するか。
  `nil' にすると、適当に十分な数で出す。
	
- SCALE

  出力の時に、 (expt 10 scale) を掛けた値を出力する。
	
- RADIX-POINT

  小数点に使用する、文字もしくは文字列。"
  (format-jp-numeral stream object
		     (flag-to-style colon-p at-sign-p)
		     :digits-after-dot digits-after-dot
		     :scale scale
		     :radix-point radix-point))

(defun wari (stream object &optional colon-p at-sign-p digits-after-dot
	     &aux (style (flag-to-style colon-p at-sign-p)))
  "This function works like `jp', but puts as a rate.
The output value is multiplied with 10, and '割' is used for the radix point.

Arguments are same as `jp'.

====

`jp' と同様だが、 割合として表示する。
10倍され、小数点に '割' を使用した値が表示される。

引数は `jp' と同様。"
  (check-type object real "jp-numeral:wari acceptable type")
  (format-jp-numeral stream object style
		     :digits-after-dot digits-after-dot
		     :scale 1
		     :radix-point (get-wari style)))

(defun yen (stream object &optional colon-p at-sign-p digits-after-dot
	    &aux (style (flag-to-style colon-p at-sign-p)))
  "This function works like `jp', but puts as a yen.
The output value is rounded on specified position, and printed until
`1' with '円', until `0.01' with '銭', and until `0.001' with '厘'.

Arguments are same as `jp', except below:

- DIGITS-AFTER-DOT

  Specifies how many digits putted after a radix point. The default is 2.
  Only one of `0`, `2`, or `3` is available.

====

`jp' と同様だが、 円として表示する。
指定した桁で丸め、 `1' の桁までは '円' で、 `0.01' の桁までは '銭' で、 `0.001' の桁は '厘' で表示する。

以下の引数以外は、`jp' の引数と同様。

- DIGITS-AFTER-DOTA

  小数点以下のどの桁まで表示するか指定する。デフォルトは 2。
  `0` , `2` , `3` のいずれかが使用できる。"
  (check-type object real "jp-numeral:yen acceptable type")
  (unless digits-after-dot
    (setf digits-after-dot 2))
  (multiple-value-bind (signum yen sen rin)
      (case digits-after-dot
	(0
	 (let ((quot-0 (round object 1)))
	   (values (signum quot-0) quot-0 0 0)))
	(2
	 (let ((quot-2 (round object 1/100)))
	   (multiple-value-bind (yen sen) (truncate quot-2 100)
	     (values (signum quot-2) yen sen 0))))
	(3
	 (let ((quot-3 (round object 1/1000)))
	   (multiple-value-bind (yen rin-rest) (truncate quot-3 1000)
	     (multiple-value-bind (sen rin) (truncate rin-rest 10)
	       (values (signum quot-3) yen sen rin)))))
	(otherwise
	 (error "digits should be 2, 3, or nil")))
    (if (zerop signum)
	(format-jp-numeral stream 0 style
			   :digits-after-dot 0
			   :scale 0
			   :radix-point (get-yen style))
	(flet ((put-ysr (n radix-point)
		 (unless (zerop n)
		   (format-jp-numeral stream (* signum (abs n))
				      style
				      :digits-after-dot 0
				      :scale 0
				      :radix-point radix-point)
		   (setf signum 1))))
	  (put-ysr yen (get-yen style))
	  (put-ysr sen (get-sen style))
	  (put-ysr rin (get-power -2 style)))))) ; 'rin' char (厘)
