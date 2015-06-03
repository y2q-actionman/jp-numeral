(in-package :jp-numeral)

;;; Conditions

(define-condition no-power-char-error (error)
  ())

(define-condition bad-format-float-error (error)
  ((original-condition :initarg :original-condition :initform nil)))


;;; Accessors to the jp-numeral-table.

(defun style-to-index (style enable-positional-p)
  (ecase style
    (:normal +TABLE-NORMAL-INDEX+)
    (:formal +TABLE-FORMAL-INDEX+)
    (:old +TABLE-OLD-INDEX+)
    (:positional (if enable-positional-p
		     +TABLE-POSITIONAL-INDEX+
		     +TABLE-NORMAL-INDEX+))))

(defun get-digit (n style)
  (assert (<= 0 n 9))
  (let ((entry (aref +digits+ n)))
    (aref entry (style-to-index style nil))))
     
(defun get-power (n style)
  (unless (<= +power-min+ n +power-max+)
    (error 'no-power-char-error))
  (alexandria:if-let ((a-entry (assoc n +power-alist+)))
    (aref (cdr a-entry)
	  (style-to-index style nil))
    (assert nil (n) "~D does not have an apropriate char" n)))

(defun get-minus-sign (style)
  (aref +minus-sign+
	(style-to-index style t)))
  
(defun get-parts-of (style)
  (aref +fraction-parts-of+
	(style-to-index style t)))

(defun get-radix-point (style)
  (aref +radix-point+
	(style-to-index style t)))

(defun get-yen (style)
  (aref +yen+ (style-to-index style nil)))

(defun get-sen (style)
  (aref +sen+ (style-to-index style nil)))

(defun get-wari (style)
  (declare (ignore style))
  +wari+)


;;; Writers

(defun print-positional (stream object style digits-after-dot scale radix-point-str)
  (let* ((trim-zero? (and (floatp object)
			  (not digits-after-dot)))
	 (digits-after-dot (or digits-after-dot
			       (- +power-min+)))
	 (lispstr
	  (ctypecase object
	    (integer (format nil "~D" object))
	    (ratio (format nil "~D/~D" (numerator object) (denominator object)))
	    (float (format nil "~,v,vF" digits-after-dot scale object)))))
    (when trim-zero?
      (setf lispstr (string-right-trim '(#\0) lispstr)))
    (loop for c across lispstr
       as jp-str = 
       (case c
	 (#\0 (get-digit 0 style))
	 (#\1 (get-digit 1 style))
	 (#\2 (get-digit 2 style))
	 (#\3 (get-digit 3 style))
	 (#\4 (get-digit 4 style))
	 (#\5 (get-digit 5 style))
	 (#\6 (get-digit 6 style))
	 (#\7 (get-digit 7 style))
	 (#\8 (get-digit 8 style))
	 (#\9 (get-digit 9 style))
	 (#\. radix-point-str)
	 (#\- (get-minus-sign style))
	 (#\/ (get-parts-of style))
	 ((#\d #\e #\f #\l #\s)
	  (assert nil (c)
		  "Exponent marker '~C' is found. This is an internal BUG of jp-numeral." c))
	 (otherwise
	  (error 'bad-format-float-error)))
       do (write-string jp-str stream))))


(defun make-digits4-string (digits4 style)
  (declare (type (integer 0 9999) digits4))
  (assert (not (eq style :positional)))
  (let ((fill-1 (ecase style
		  (:normal nil)
		  ((:formal :old) t))))
    (with-output-to-string (buf-stream)
      (labels ((put-n (str)
		 (write-string str buf-stream))
	       (put-100-1000 (digit pow)
		 (unless (zerop digit)
		   (when (or (<= 2 digit 9)
			     fill-1)
		     (put-n (get-digit digit style)))
		   (put-n (get-power pow style)))))
	(multiple-value-bind (d3 d3-rest) (floor digits4 1000)
	  (put-100-1000 d3 3)
	  (multiple-value-bind (d2 d2-rest) (floor d3-rest 100)
	    (put-100-1000 d2 2)
	    ;; 0 - 99
	    (multiple-value-bind (d1 d0) (floor d2-rest 10)
	      (unless (zerop d1)
		(when (<= 2 d1 9)      ; fill-1 is not respected here.
		  (put-n (get-digit d1 style)))
		(put-n (get-power 1 style)))
	      (unless (zerop d0)
		(put-n (get-digit d0 style))))))))))

(defun print-jp-integer (stream object style)
  (declare (type (integer 0) object))
  (loop with strs = nil
     for power from 0 by 4
     for (rest digits4) = (multiple-value-list (floor object 10000))
     then (multiple-value-list (floor rest 10000))
     as digits4-str = (make-digits4-string digits4 style)
     when (plusp (length digits4-str))
     do (when (plusp power)
	  (push (get-power power style) strs))
     and do (push digits4-str strs)
     while (> rest 0)
     finally (mapc #'(lambda (s) (write-string s stream)) strs)))


(defun print-fraction (stream object style digits-after-dot scale radix-point-str)
  (declare (type float object))
  (handler-bind ((error #'(lambda (c)
			    (error 'bad-format-float-error :original-condition c))))
    (let* ((ratio (if digits-after-dot
		      (rational object)
		      (rationalize object)))
	   ;; -- support 100000000000000.1d0
	   (fp-error (multiple-value-bind (_scaled expon _sign)
			 (integer-decode-float object)
		       (declare (ignore _scaled _sign))
		       (expt (float-radix object) expon)))
	   (scaled-ratio (* ratio (expt 10 scale)))
	   (print-power-min (if digits-after-dot (- digits-after-dot)
				+power-min+)))
      (multiple-value-bind (int-part frac-part)
	  (truncate scaled-ratio)
	(print-jp-integer stream int-part style)
	(when (plusp int-part)
	  (write-string radix-point-str stream))
	(loop with printed = 0
	   for power downfrom -1 to print-power-min
	   for current-scale = 1/10 then (/ current-scale 10) ; (expt 10 power)
	   while (< fp-error current-scale)
	   as current-digit = 
	   (loop for i from 0 to 9
	      for sum = printed then next-sum
	      for next-sum from (+ printed current-scale) by current-scale
	      until (and (<= sum frac-part)
			 (< frac-part next-sum))
	      finally
	      (setf printed sum)
	      (return i))
	   do (when (/= 0 current-digit)
		(write-string (get-digit current-digit style) stream)
		(write-string (get-power power style) stream))
	   while (< printed frac-part))))))


(defun flag-to-style (colon-p at-sign-p)
  (cond ((and colon-p at-sign-p) :positional)
	(colon-p :formal)
	(at-sign-p :old)
	(t :normal)))

(defun pprint-jp-numeral (stream object &optional colon-p at-sign-p
			  digits-after-dot scale radix-point-char
			  &aux (style (flag-to-style colon-p at-sign-p)))
  (unless (numberp object)
    (error "~A is not an expected type for jp-numeral" (type-of object)))
  (prog ((*print-base* 10) ; *print-base* must be 10 for jp-numeral
	 (scale (or scale 0))
	 (radix-point-str (etypecase radix-point-char
			    (string radix-point-char)
			    (character (string radix-point-char))
			    (null (get-radix-point style))))
	 (buf (make-array '(1) :element-type 'character :fill-pointer 0 :adjustable t)))
   try-again
   (handler-case
       (with-output-to-string (buf-stream buf)
	 (case style
	   (:positional
	    (print-positional stream object style digits-after-dot scale radix-point-str))
	   (otherwise
	    (when (minusp object)
	      (write-string (get-minus-sign style) buf-stream))
	    (ctypecase object
	      (integer
	       (print-jp-integer buf-stream (abs (* object (expt 10 scale))) style)
	       (when radix-point-char
		 (write-string radix-point-str buf-stream)))
	      (ratio
	       (let ((object (* object (expt 10 scale))))
		 (print-jp-integer buf-stream (denominator object) style)
		 (write-string (get-parts-of style) buf-stream)
		 (print-jp-integer buf-stream (abs (numerator object)) style)))
	      (float
	       (print-fraction buf-stream object style digits-after-dot scale radix-point-str)))
	    (unless (plusp (length buf))
	      (write-string (get-digit 0 style) buf-stream)))))
     (no-power-char-error ()
       ;; Decimal power chars are exhausted. Use positional.
       (assert (not (eq style :positional)))
       (setf style :positional)
       (setf (fill-pointer buf) 0)
       (go try-again))
     (bad-format-float-error ()
       ;; Infinity or NaN.
       ;; TODO: Use right floating-point handlings.
       (setf style nil)
       (setf (fill-pointer buf) 0)
       (format buf "~A" object)))
   ;; Final output.
   (write-string buf stream))
  style) ; If this is not expected one by the caller, means alternative methods used.


;;; cl:format interface

(defun J (stream object &optional colon-p at-sign-p
			  digits-after-dot scale radix-point-char)
  (pprint-jp-numeral stream object colon-p at-sign-p
		     digits-after-dot scale radix-point-char))
  
(defun wari (stream object &optional colon-p at-sign-p
			     digits-after-dot)
  (assert (= (length +wari+) 1))
  (pprint-jp-numeral stream object colon-p at-sign-p
		     digits-after-dot 1 (char +wari+ 0)))

(defun W (stream object &optional colon-p at-sign-p
			  digits-after-dot)
  (wari stream object colon-p at-sign-p digits-after-dot))

(defun sen (stream object &optional colon-p at-sign-p digits-after-dot)
  (unless digits-after-dot
    (setf digits-after-dot 2))
  (unless (<= 2 digits-after-dot 3)
    (error "Sen: digits should be 2 or 3"))
  (let* ((style (flag-to-style colon-p at-sign-p))
	 (yen-str (get-yen style))
	 (sen-str (get-sen style))
	 (rin-str (get-power -2 style)))
    (when (eq (pprint-jp-numeral stream object colon-p at-sign-p
				 0 0 yen-str)
	      style)
      (assert (>= digits-after-dot 2))
      (pprint-jp-numeral stream (rem (* 100 object) 100) colon-p at-sign-p
			 0 0 sen-str)
      (when (>= digits-after-dot 3)
	(pprint-jp-numeral stream (rem (* 1000 object) 10) colon-p at-sign-p
			   0 0 rin-str)))))
