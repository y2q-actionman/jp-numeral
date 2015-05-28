(in-package :jp-numeral)

;;; Conditions

(define-condition overflow-error (error)
  ())

(define-condition underflow-error (error)
  ())

(define-condition exponent-marker-found-condition (serious-condition)
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
  (when (>= n (+ +power-max+ 4))
    (error 'overflow-error))
  (when (< n +power-min+)
    (error 'underflow-error))
  (alexandria:if-let ((a-entry (assoc n +power-alist+)))
    (aref (cdr a-entry)
	  (style-to-index style nil))))

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


;;; Writers

(defun translate-char (char style)
  (case char
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
    (#\- (get-minus-sign style))
    (#\/ (get-parts-of style))
    ((#\d #\e #\f #\l #\s)
     (error 'exponent-marker-found-condition))
    (otherwise
     (error 'bad-format-float-error))))


(defun make-digits4-string (digits4 style)
  (declare (type (integer 0 9999) digits4))
  (let ((fill-1 (ecase style
		  (:normal nil)
		  ((:formal :old) t)))
	(buf (make-array '(9) :element-type 'character :fill-pointer 0)))
    (labels ((put-n (str)
	       (loop for s across str
		  do (vector-push-extend s buf)))
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
	      (when (<= 2 d1 9)	       ; fill-1 is not respected here.
		(put-n (get-digit d1 style)))
	      (put-n (get-power 1 style)))
	    (unless (zerop d0)
	      (put-n (get-digit d0 style)))))))
    buf))

(defun make-integer-string (object style)
  (declare (type (integer 0) object))
  ;; TODO: cleanup if object == 0
  ;; - make-integer-string -> ""
  ;; - make-positional-integer-string -> "〇"
  (when (eq style :positional)
    (return-from make-integer-string
      (make-positional-integer-string object)))
  (loop with strs = nil
     for power from 0 by 4
     for (rest digits4) = (multiple-value-list (floor object 10000))
     then (multiple-value-list (floor rest 10000))
     as digits4-str = (make-digits4-string digits4 style)
     as power-str = (get-power power style)
     when (plusp (length digits4-str))
     do (when power-str
	  (push power-str strs))
       (push digits4-str strs)
     while (> rest 0)
     finally (return strs)))

(defun make-fractional-part-string (object style digits-after-dot scale)
  (declare (type float object))
  ;; TODO: treat infinities
  (loop with frac-part-l-str = (format nil "~,v,vF" digits-after-dot scale object)
     for i from (1+ (position #\. frac-part-l-str)) below (length frac-part-l-str)
     as c = (char frac-part-l-str i)
     for power downfrom -1
     when (char/= #\0 c)
     collect (translate-char c style)
     and collect (get-power power style)))


(defun print-positional-jp-numeral (stream object &optional digits-after-dot scale decimal-mark)
  (unless digits-after-dot
    (setf digits-after-dot nil))
  (unless scale
    (setf scale 0))
  (unless decimal-mark
    (setf decimal-mark (get-radix-point-char :positional)))
  (flet ((recurse (object)
	   (print-positional-jp-numeral
	    stream object digits-after-dot scale decimal-mark)))
    (ctypecase object
      (integer
       (loop with lispstr = (format nil "~D" object)
	  for c across lispstr
	  do (write-string (translate-char c :positional)
			   stream)))
      (ratio
       (recurse (numerator object))
       (write-string (get-parts-of :positional) stream)
       (recurse (denominator object)))
      (float
       (recurse (truncate object))
       (write-char decimal-mark stream)
       ;; TODO: treat infinities and the exponent marker
       (loop with frac-part-l-str = (format nil "~,v,vF" digits-after-dot scale object)
	  for i from (1+ (position #\. frac-part-l-str)) below (length frac-part-l-str)
	  as c = (char frac-part-l-str i)
	  do (write-string (translate-char c :positional) stream))))))

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
  (when (eq style :positional)
    (return-from pprint-jp-numeral
      (print-positional-jp-numeral stream object digits-after-dot scale decimal-mark)))
  (unless digits-after-dot
    (setf digits-after-dot (- +power-min+)))
  (unless scale
    (setf scale 0))
  (unless decimal-mark
    (setf decimal-mark (get-radix-point-char style)))
  (let ((outputs nil))			; TODO: use with-output-to-string as a buffer.
    (when (minusp object)
      (alexandria:appendf outputs (get-minus-sign style)))
    (ctypecase object
      (integer
       ;; TODO: catch overflow-error, and use alternative string.
       (alexandria:appendf outputs
			   (make-integer-string (abs object) style)))
      (ratio
       ;; (assert (plusp (denominator object))) ; Hyperspec 12.1.3.2
       (let ((numerator-strs (make-integer-string (abs (numerator object)) style))
	     (parts-of-str (get-parts-of style))
	     (denominator-strs (make-integer-string (denominator object) style)))
	 ;; sign is printed as a mixed-fraction style.
	 (alexandria:appendf outputs
			     denominator-strs
			     (list parts-of-str)
			     numerator-strs)))
      (float
       (let ((int-strs (make-integer-string (abs (truncate object)) style))
	     (float-strs (make-fractional-part-string object style digits-after-dot scale)))
	 (alexandria:appendf outputs int-strs)
	 ;; FIXME: should print "〇割一分" ?
	 (when outputs
	   (alexandria:appendf outputs (list (string decimal-mark))))
	 (alexandria:appendf outputs float-strs))))
    (unless outputs
      (alexandria:appendf outputs (get-digit 0 style)))
    ;; write things
    (loop for obj in outputs
       do (write-string obj stream))))


;;; cl:format interface

;; TODO: rewrite with appropriate args.
(setf (fdefinition 'j)
      #'pprint-jp-numeral)
