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


;;; Utils

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


;;; Writers

(defgeneric write-jp-numeral
    (object style stream
	    &key digits-after-dot scale radix-point-string
	    radix-point-required-p)
  (:method (object style stream &key &allow-other-keys)
    (declare (ignore object style stream))
    (error 'not-formattable-error)))


(defmethod write-jp-numeral :around ((object rational) style stream
				     &rest args
				     &key scale &allow-other-keys)
  (let* ((scaled-object (* object (expt 10 scale)))
	 (original-type (type-of object))
	 (scaled-type (type-of scaled-object)))
    ;; If they are not same type, dispatch the object again.
    (if (and (subtypep original-type scaled-type)
	     (subtypep scaled-type original-type))
	(apply #'call-next-method scaled-object style stream
	       :scale 0
	       args)
	(apply #'write-jp-numeral scaled-object style stream
	       :scale 0
	       args))))


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

(defun write-positional-from-string (lispstr stream style
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

(defmethod write-jp-numeral ((object integer) (style (eql :positional)) stream
			     &key digits-after-dot scale radix-point-string
			     radix-point-required-p)
  (declare (ignore digits-after-dot)
	   (ignorable scale))
  (assert (zerop scale))
  (write-positional-from-string
   (format nil "~D~@[.~]" object radix-point-required-p)
   stream style radix-point-string))

(defmethod write-jp-numeral ((object ratio) (style (eql :positional)) stream
			     &key digits-after-dot scale radix-point-string
			     radix-point-required-p)
  (declare (ignore digits-after-dot)
	   (ignorable scale))
  (assert (zerop scale))
  (write-positional-from-string
   (format nil "~D/~D~@[.~]" (numerator object) (denominator object) radix-point-required-p)
   stream style radix-point-string))


(defun stringify-float (flt digits-after-dot scale)
  (let ((width (float-sufficient-width flt)))
    (when (and (integerp digits-after-dot)
	       (plusp digits-after-dot))
      (incf width digits-after-dot))
    ;; width is required for working 'scale'.
    ;; (If both width and digits-after-dot are nil, it does not work..)
    (format nil "~v,v,vF" width digits-after-dot scale flt)))

(defmethod write-jp-numeral ((object float) (style (eql :positional)) stream
			     &key digits-after-dot scale radix-point-string
			     radix-point-required-p)
  (declare (ignore radix-point-required-p)) ; radix point is always put.
  (write-positional-from-string
   (stringify-float object digits-after-dot scale)
   stream style radix-point-string))


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

(defmethod write-jp-numeral ((object integer) style stream
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

(defmethod write-jp-numeral ((object ratio) style stream
			     &key digits-after-dot scale radix-point-string
			     radix-point-required-p)
  (declare (ignore digits-after-dot radix-point-string
		   radix-point-required-p)
	   (ignorable scale))
  (assert (zerop scale))
  (when (minusp object)
    (write-string (get-minus-sign style) stream))
  (print-jp-plus-integer stream (denominator object) style)
  (write-string (get-parts-of style) stream)
  (print-jp-plus-integer stream (abs (numerator object)) style)
  (when radix-point-required-p
    (write-string radix-point-string stream)))


(defmethod write-jp-numeral ((object float) style stream
			     &key digits-after-dot scale radix-point-string
			     radix-point-required-p)
  (when (minusp object)
    (write-string (get-minus-sign style) stream)
    (setf object (- object)))
  (let* ((lispstr (stringify-float object digits-after-dot scale))
	 (dot-pos (let ((pos (position #\. lispstr)))
		    (unless pos
		      (error 'not-formattable-error))
		    pos))
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


(defun flag-to-style (colon-p at-sign-p)
  (cond ((and colon-p at-sign-p) :positional)
	(colon-p :formal)
	(at-sign-p :old)
	(t :normal)))

(defun pprint-jp-numeral (o-stream object &optional colon-p at-sign-p
			  digits-after-dot scale radix-point
			  &aux (style (flag-to-style colon-p at-sign-p)))
  (unless (numberp object)
    (error "~A is not an expected type for jp-numeral" (type-of object)))
  (prog ((*print-base* 10)    ; *print-base* must be 10 for jp-numeral
	 (scale (or scale 0))
	 (radix-point-str (etypecase radix-point
			    (string radix-point)
			    (character (string radix-point))
			    (null (get-radix-point style))))
	 (buf (make-array '(1) :element-type 'character :fill-pointer 0 :adjustable t)))
   try-again
   (handler-case
       (with-output-to-string (stream buf)
	 (write-jp-numeral object style stream
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
   (write-string buf o-stream))
  style) ; If this is not expected one by the caller, means alternative methods used.


;;; cl:format interface

(defun JP (&rest args)
  (apply #'pprint-jp-numeral args))
  
(defun J (&rest args)
  (apply #'pprint-jp-numeral args))
  
(defun wari (stream object &optional colon-p at-sign-p digits-after-dot
	     &aux (style (flag-to-style colon-p at-sign-p)))
  (pprint-jp-numeral stream object colon-p at-sign-p
		     digits-after-dot 1 (get-wari style)))

(defun W (&rest args)
  (apply #'wari args))

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

(defun Y (&rest args)
  (apply #'yen args))
