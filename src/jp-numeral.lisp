(in-package :jp-numeral)

;;; Conditions

(define-condition no-power-char-error (error)
  ())

(define-condition not-formattable-error (error)
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

(defun translate-digit (c style)
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

(defun print-positional (stream object style digits-after-dot scale radix-point-str)
  (loop with lispstr
       = (typecase object
	   (integer (format nil "~D" object))
	   (ratio (format nil "~D/~D" (numerator object) (denominator object)))
	   (float (format nil "~,v,vF" digits-after-dot scale object))
	   (t (error 'not-formattable-error)))
     for c across lispstr
     as jp-str = 
     (case c
       ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
	(translate-digit c style))
       (#\. radix-point-str)
       (#\- (get-minus-sign style))
       (#\/ (get-parts-of style))
       (otherwise
	(error 'not-formattable-error)))
     do (write-string jp-str stream)))


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

(defun print-jp-integer (stream object style scale)
  (declare (type integer object))
  (setf object (* object (expt 10 scale)))
  (loop with strs = nil
     for power from 0 by 4
     for (rest digits4) = (multiple-value-list (floor object 10000))
     then (multiple-value-list (floor rest 10000))
     as digits4-str = (make-digits4-string digits4 style)
     when (plusp (length digits4-str))
     do (when (plusp power)
	  (push (get-power power style) strs))
       (push digits4-str strs)
     while (plusp rest)
     finally (mapc #'(lambda (s) (write-string s stream)) strs)))

(defun print-fractional-part (stream object style digits-after-dot scale)
  (let* ((trim-zero? (not digits-after-dot))
	 (digits-after-dot (or digits-after-dot
			       (- +power-min+)))
	 (scale (or scale 0))
	 (lispstr (format nil "~,v,vF" digits-after-dot scale object)))
    (when trim-zero?
      (setf lispstr (string-right-trim '(#\0) lispstr)))
    (loop for i from (1+ (position #\. lispstr)) below (length lispstr)
       as c = (aref lispstr i)
       for power downfrom -1 to (- digits-after-dot)
       unless (digit-char-p c)
       do (error 'not-formattable-error)
       if (char/= #\0 c)
       do (write-string (translate-digit c style) stream)
       (write-string (get-power power style) stream))))


(defun flag-to-style (colon-p at-sign-p)
  (cond ((and colon-p at-sign-p) :positional)
	(colon-p :formal)
	(at-sign-p :old)
	(t :normal)))

(defun pprint-jp-numeral (o-stream object &optional colon-p at-sign-p
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
       (with-output-to-string (stream buf)
	 (case style
	   (:positional
	    (print-positional stream object style digits-after-dot scale radix-point-str))
	   (otherwise
	    (when (minusp object)
	      (write-string (get-minus-sign style) stream)
	      (setf object (- object)))
	    (typecase object
	      (integer
	       (print-jp-integer stream object style scale)
	       (when (zerop object)
		 (assert (zerop (length buf)))
		 (write-string (get-digit 0 style) stream))
	       (when radix-point-char
		 (write-string radix-point-str stream)))
	      (ratio
	       (assert (not (zerop object)))
	       (let ((object (* object (expt 10 scale))))
		 (print-jp-integer stream (denominator object) style 0)
		 (write-string (get-parts-of style) stream)
		 (print-jp-integer stream (abs (numerator object)) style 0)))
	      (float
	       (handler-bind ((error #'(lambda (c) ; Infinity or NaN.
					 (error 'not-formattable-error c))))
		 (let* ((int-part (floor object))
			(int-buf (with-output-to-string (s)
				   (print-jp-integer s int-part style scale)))
			(frac-buf (with-output-to-string (s)
				    (print-fractional-part s object style digits-after-dot scale))))
		   (write-string int-buf stream)
		   ;; prints '0' if needed
		   (when (and (zerop int-part)
			      (or radix-point-char
				  (zerop (length frac-buf))))
		     (write-string (get-digit 0 style) stream))
		   ;; prints '.' if needed
		   (when (or radix-point-char
			     (and (plusp (length int-buf))
				  (plusp (length frac-buf))))
		     (write-string radix-point-str stream))
		   (write-string frac-buf stream))))
	      (t			; complex etc.
	       (error 'not-formattable-error))))))
     (no-power-char-error ()
       ;; Decimal power chars are exhausted. Use positional.
       (assert (not (eq style :positional)))
       (setf style :positional)
       (setf (fill-pointer buf) 0)
       (go try-again))
     (not-formattable-error ()
       (setf style nil)
       (setf (fill-pointer buf) 0)
       (format buf "~A" object)))
   ;; Final output.
   (write-string buf o-stream))
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

(defun yen (stream object &optional colon-p at-sign-p digits-after-dot
	    &aux (style (flag-to-style colon-p at-sign-p)))
  (case digits-after-dot
    ((nil)
     (setf digits-after-dot 2))
    ((2 3)
     t)
    (otherwise
     (error "digits should be 2, 3, or nil")))
  (when (eq (pprint-jp-numeral stream object colon-p at-sign-p
			       0 0 (get-yen style))
	    style)
    (setf object (abs object))
    (when (>= digits-after-dot 2)
      (let ((sen-num (rem (* 100 object) 100)))
	(when (plusp sen-num)
	  (pprint-jp-numeral stream sen-num colon-p at-sign-p 0 0 "")
	  (write-string (get-sen style) stream))))
    (when (>= digits-after-dot 3)
      (let ((rin-num (rem (* 1000 object) 10)))
	(when (plusp rin-num)
	  (pprint-jp-numeral stream rin-num colon-p at-sign-p 0 0 "")
	  (write-string (get-power -2 style) stream))))))

(defun Y (stream object &optional colon-p at-sign-p digits-after-dot)
  (yen stream object colon-p at-sign-p digits-after-dot))
