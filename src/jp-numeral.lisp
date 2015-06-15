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
  (unless (<= +power-min+ n +power-max+)
    (error 'no-power-char-error))
  (alexandria:if-let ((a-entry (assoc n +power-alist+)))
    (aref (cdr a-entry)
	  (style-to-index style))
    (assert nil (n) "~D does not have an apropriate char" n)))

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
       = (with-standard-io-syntax
	   (typecase object
	     (integer (format nil "~D" object))
	     (ratio (format nil "~D/~D" (numerator object) (denominator object)))
	     (float (format nil "~,v,vF" digits-after-dot scale object))
	     (t (error 'not-formattable-error))))
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
  (let ((fill-1 (ecase style
		  (:normal nil)
		  ((:formal :old) t))))
    (with-output-to-string (stream)
      (flet ((put-digit (digit pow)
	       (ecase digit
		 ((2 3 4 5 6 7 8 9)
		  (write-string (get-digit digit style) stream)
		  (write-string (get-power pow style) stream))
		 (1 (ecase pow
		      ((2 3)
		       (when fill-1
			 (write-string (get-digit digit style) stream)))
		      (1 nil)
		      (0 (write-string (get-digit digit style) stream)))
		    (write-string (get-power pow style) stream))
		 (0 nil))))
	(multiple-value-bind (d3 d3-rest) (floor digits4 1000)
	  (put-digit d3 3)
	  (multiple-value-bind (d2 d2-rest) (floor d3-rest 100)
	    (put-digit d2 2)
	    (multiple-value-bind (d1 d0) (floor d2-rest 10)
	      (put-digit d1 1)
	      (put-digit d0 0))))))))

(defun print-jp-plus-integer (stream object style)
  (declare (type integer object))
  (loop with strs = nil
     for power from 0 by 4
     for (rest digits4) = (multiple-value-list (floor object 10000))
     then (multiple-value-list (floor rest 10000))
     as digits4-str = (make-digits4-string digits4 style)
     when (plusp (length digits4-str))
     do (push (get-power power style) strs)
       (push digits4-str strs)
     while (plusp rest)
     finally (mapc #'(lambda (s) (write-string s stream)) strs)))


(defun print-jp-fraction (stream object style digits-after-dot scale
			  radix-point-str radix-point-required-p)
  (when (minusp object)
    (write-string (get-minus-sign style) stream)
    (setf object (- object)))
  (let* ((trim-zero? (not digits-after-dot))
	 (digits-after-dot (or digits-after-dot
			       (prog1 (- +power-min+)
				 (when (< (log object 10) +power-min+)
				   (error 'not-formattable-error)))))
	 (lispstr (let ((str (with-standard-io-syntax
			       (format nil "~,v,vF" digits-after-dot scale object))))
		    (if trim-zero?
			(string-right-trim '(#\0) str)
			str)))
	 (dot-pos (let ((pos (position #\. lispstr)))
		    (unless pos
		      (error 'not-formattable-error))
		    pos))
	 (int-part (parse-integer lispstr :end dot-pos))
	 (frac-part-strlen (- (length lispstr) (1+ dot-pos))))
    ;; int part
    (print-jp-plus-integer stream int-part style)
    ;; prints '0' if needed
    (when (and (zerop int-part)
	       (or radix-point-required-p
		   (zerop frac-part-strlen)))
      (write-string (get-digit 0 style) stream))
    ;; prints '.' if needed
    (when (or radix-point-required-p
	      (and (not (zerop int-part))
		   (plusp frac-part-strlen)))
      (write-string radix-point-str stream))
    ;; frac part
    (loop for i from (1+ dot-pos) below (length lispstr)
       as c = (aref lispstr i)
       for power downfrom -1
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
			  digits-after-dot scale radix-point-arg
			  &aux (style (flag-to-style colon-p at-sign-p)))
  (unless (numberp object)
    (error "~A is not an expected type for jp-numeral" (type-of object)))
  (prog ((*print-base* 10)    ; *print-base* must be 10 for jp-numeral
	 (scale (or scale 0))
	 (radix-point-str (etypecase radix-point-arg
			    (string radix-point-arg)
			    (character (string radix-point-arg))
			    (null (get-radix-point style))))
	 (buf (make-array '(1) :element-type 'character :fill-pointer 0 :adjustable t)))
   try-again
   (handler-case
       (with-output-to-string (stream buf)
	 (flet ((apply-scale (type)
		  (unless (zerop scale)
		    (setf object (* object (expt 10 scale))
			  scale 0)
		    (unless (typep object type)
		      (go try-again)))))
	   (cond
	     ((eq style :positional)
	      (print-positional stream object style digits-after-dot scale radix-point-str))
	     ((integerp object)
	      (apply-scale 'integer)
	      (when (minusp object)
		(write-string (get-minus-sign style) stream))
	      (if (zerop object)
		  (write-string (get-digit 0 style) stream)
		  (print-jp-plus-integer stream (abs object) style))
	      (when radix-point-arg
		(write-string radix-point-str stream)))
	     ((rationalp object)
	      (assert (not (zerop object)))
	      (apply-scale 'ratio)
	      (when (minusp object)
		(write-string (get-minus-sign style) stream))
	      (print-jp-plus-integer stream (denominator object) style)
	      (write-string (get-parts-of style) stream)
	      (print-jp-plus-integer stream (abs (numerator object)) style))
	     ((floatp object)
	      (print-jp-fraction stream object style digits-after-dot scale
				 radix-point-str radix-point-arg))
	     (t				; complex etc.
	      (error 'not-formattable-error)))))
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
   (write-string buf o-stream))
  style) ; If this is not expected one by the caller, means alternative methods used.


;;; cl:format interface

(defun JP (stream object &optional colon-p at-sign-p
			  digits-after-dot scale radix-point-arg)
  (pprint-jp-numeral stream object colon-p at-sign-p
		     digits-after-dot scale radix-point-arg))
  
(defun J (stream object &optional colon-p at-sign-p
			  digits-after-dot scale radix-point-arg)
  (pprint-jp-numeral stream object colon-p at-sign-p
		     digits-after-dot scale radix-point-arg))
  
(defun wari (stream object &optional colon-p at-sign-p digits-after-dot
	     &aux (style (flag-to-style colon-p at-sign-p)))
  (pprint-jp-numeral stream object colon-p at-sign-p
		     digits-after-dot 1 (get-wari style)))

(defun W (stream object &optional colon-p at-sign-p
			  digits-after-dot)
  (wari stream object colon-p at-sign-p digits-after-dot))

(defun yen (stream object &optional colon-p at-sign-p digits-after-dot
	    &aux (style (flag-to-style colon-p at-sign-p)))
  (flet ((print-ysr (signum yen sen rin)
	   (when (zerop signum)
	     (return-from print-ysr
	       (pprint-jp-numeral stream 0 colon-p at-sign-p
				  0 0 (get-yen style))))
	   (when (minusp signum)
	     (write-string (get-minus-sign style) stream))
	   (unless (zerop yen)
	     (pprint-jp-numeral stream (abs yen) colon-p at-sign-p
				0 0 (get-yen style)))
	   (unless (zerop sen)
	     (pprint-jp-numeral stream (abs sen) colon-p at-sign-p
				0 0 (get-sen style)))
	   (unless (zerop rin)
	     (pprint-jp-numeral stream (abs rin) colon-p at-sign-p
				0 0 (get-power -2 style))))) ; 'rin' char (厘) 
    (case digits-after-dot
      ((nil 2)
       (setf digits-after-dot 2)
       (let ((quot-2 (round object 1/100)))
	 (multiple-value-bind (yen sen) (truncate quot-2 100)
	   (print-ysr (signum quot-2) yen sen 0))))
      (3
       (let ((quot-3 (round object 1/1000)))
	 (multiple-value-bind (yen rin-rest) (truncate quot-3 1000)
	   (multiple-value-bind (sen rin) (truncate rin-rest 10)
	     (print-ysr (signum quot-3) yen sen rin)))))
      (otherwise
       (error "digits should be 2, 3, or nil")))))

(defun Y (stream object &optional colon-p at-sign-p digits-after-dot)
  (yen stream object colon-p at-sign-p digits-after-dot))
