(in-package :jp-numeral)

;;; Utils

(defun positive-infinite-p (n)
  (etypecase n
    (short-float (> n most-positive-short-float))
    (single-float (> n most-positive-single-float))
    (double-float (> n most-positive-double-float))
    (long-float (> n most-positive-long-float))))

(defun negative-infinite-p (n)
  (etypecase n
    (short-float (< n most-negative-short-float))
    (single-float (< n most-negative-single-float))
    (double-float (< n most-negative-double-float))
    (long-float (< n most-negative-long-float))))

;;; Conditions

(define-condition no-power-char-error (error)
  ())

(define-condition bad-format-float-error (error)
  ())


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

(defun get-radix-point-char (style)
  (let ((s (aref +radix-point+
		 (style-to-index style t))))
    (assert (= (length s) 1))
    (char s 0)))

(defun get-infinity (style)
  (aref +infinite+ (style-to-index style t)))

(defun get-nan (style)
  (aref +nan+ (style-to-index style t)))


;;; Writers

(defun translate-char (char style)
  (ecase char
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
  (case style
    (:positional
     (loop with lispstr = (format nil "~D" object)
	for c across lispstr
	do (write-string (translate-char c style) stream)))
    (otherwise
     (loop with strs = nil
	for power from 0 by 4
	for (rest digits4) = (multiple-value-list (floor object 10000))
	then (multiple-value-list (floor rest 10000))
	as digits4-str = (make-digits4-string digits4 style)
	as power-str = (if (plusp power)
			   (get-power power style) nil)
	when (plusp (length digits4-str))
	do (when power-str
	     (push power-str strs))
	and do (push digits4-str strs)
	while (> rest 0)
	finally (mapc #'(lambda (s) (write-string s stream)) strs)))))

(defun print-fractional-part (stream object style digits-after-dot scale)
  ;; (declare (type float object))
  (let* ((trim-zero? (if digits-after-dot t nil))
	 (digits-after-dot (or digits-after-dot
			       (- +power-min+)))
	 (scale (or scale 0))
	 (frac-part-l-str (format nil "~,v,vF" digits-after-dot scale object)))
    (when trim-zero?
      (setf frac-part-l-str (string-right-trim '(#\0) frac-part-l-str)))
    (loop for i from (1+ (position #\. frac-part-l-str)) below (length frac-part-l-str)
       as c = (aref frac-part-l-str i)
       for power downfrom -1
       unless (digit-char-p c)
       do (case c
	    ((#\- #\/)
	     (assert nil (c) "~C should be treated around this function." c))
	    ((#\d #\e #\f #\l #\s)
	     (assert nil (c)
		     "Exponent marker '~C' is found. This is an internal BUG of jp-numeral." c))
	    (otherwise
	     (error 'bad-format-float-error)))
       if (eq style :positional)
       do (write-string (translate-char c style) stream)
       else if (char/= #\0 c)
       do (write-string (translate-char c style) stream)
       (write-string (get-power power style) stream))))

;; This does not use Lisp printer.
#+ignore
(defun print-fractional-2 (stream object style digits-after-dot scale)
  (let* ((ratio (if digits-after-dot
		    (rational object)
		    (rationalize object)))
	 (scaled-ratio (* ratio (expt 10 (or scale 0))))
	 (print-power-min (if digits-after-dot (- digits-after-dot)
			      +power-min+)))
    (multiple-value-bind (int-part frac-part)
	;; TODO: treat signals 
	(floor scaled-ratio)
      (print-jp-integer stream int-part style)
      (when (plusp int-part)
	;; TODO: use decimal-mark arg.
	(write-char (get-radix-point-char style) stream))
      (loop with printed = 0
	 for power downfrom -1 to print-power-min
	 for current-scale = 1/10 then (/ current-scale 10) ; (expt 10 power)
	 as current-digit = 
	 (loop for i from 0 to 9
	    for sum = printed then next-sum
	    for next-sum from (+ printed current-scale) by current-scale
	    until (and (<= sum frac-part)
		       (< frac-part next-sum))
	    finally
	    (setf printed sum)
	    (return i))
	 do (case style
	      (:positional
	       (write-string (get-digit current-digit style) stream))
	      (otherwise
	       (when (/= 0 current-digit)
		 (write-string (get-digit current-digit style) stream)
		 (write-string (get-power power style) stream))))
	 while (< printed frac-part)))))
			 

(defun pprint-jp-numeral (stream object &optional colon-p at-sign-p
			  digits-after-dot scale decimal-mark
			  &aux (style (cond ((and colon-p at-sign-p) :positional)
					    (colon-p :formal)
					    (at-sign-p :old)
					    (t :normal))))
  (unless (numberp object)
    (error "~A is not an expected type for jp-numeral" (type-of object)))
  (unless (= *print-base* 10)
    (error "*print-base* must be 10 for jp-numeral"))
  (prog ((buf (make-array '(1) :element-type 'character :fill-pointer 0 :adjustable t)))
   try-again
   (handler-case
       (with-output-to-string (buf-stream buf)
	 (when (minusp object)
	   (write-string (get-minus-sign style) buf-stream))
	 (ctypecase object
	   (integer
	    (print-jp-integer buf-stream (abs object) style))
	   (ratio
	    (case style
	      (:positional
	       (print-jp-integer buf-stream (abs (numerator object)) style)
	       (write-string (get-parts-of style) buf-stream)
	       (print-jp-integer buf-stream (denominator object) style))
	      (t
	       (print-jp-integer buf-stream (denominator object) style)
	       (write-string (get-parts-of style) buf-stream)
	       (print-jp-integer buf-stream (abs (numerator object)) style))))
	   (float
	    (let ((int-part
		   (handler-case (truncate object)
		     (error ()
		       (error 'bad-format-float-error)))))
	      (print-jp-integer buf-stream (abs int-part) style))
	    (when (plusp (length buf))
	      (write-char (or decimal-mark (get-radix-point-char style))
			  buf-stream))
	    (print-fractional-part buf-stream object style digits-after-dot scale)))
	 (unless (plusp (length buf))
	   (write-string (get-digit 0 style) buf-stream)))
     (no-power-char-error ()
       ;; Decimal chars are exhausted. Use positional.
       (assert (not (eq style :positional)))
       (setf style :positional)
       (setf (fill-pointer buf) 0)
       (go try-again))
     (bad-format-float-error ()
       ;; Infinity or NaN.
       ;; TODO: this routine is not good. Should use right floating-point handlings.
       (setf (fill-pointer buf) 0)
       (let ((pos-inf? (positive-infinite-p object))
	     (neg-inf? (negative-infinite-p object)))
	 (cond ((and pos-inf? (not neg-inf?))
		(write-string (get-infinity style)))
	       ((and (not pos-inf?) neg-inf?)
		(write-string (get-minus-sign style))
		(write-string (get-infinity style)))
	       (t
		(write-string (get-nan style)))))
       (go final-output)))
   final-output
   (write-string buf stream)))


;;; cl:format interface

(defun J (stream object &optional colon-p at-sign-p
			  digits-after-dot scale decimal-mark)
  (pprint-jp-numeral stream object colon-p at-sign-p
		     digits-after-dot scale decimal-mark))
  
(defun wari (stream object &optional colon-p at-sign-p
			     digits-after-dot)
  (assert (= (length +wari+) 1))
  (pprint-jp-numeral stream object colon-p at-sign-p
		     digits-after-dot 1 (char +wari+ 0)))

(defun W (stream object &optional colon-p at-sign-p
			  digits-after-dot)
  (wari stream object colon-p at-sign-p digits-after-dot))
