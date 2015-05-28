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

(defun get-radix-point (style)
  (aref +radix-point+
	(style-to-index style t)))


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


(defun make-positional-integer-string (object)
  (declare (type integer object))
  (loop with lispstr = (format nil "~D" object)
     for c across lispstr
     collect (translate-char c :positional)))

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
     do (push power-str strs)
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
     ;; TODO: cleanup
     when (or (eq style :positional)
	      (char/= #\0 c))
     collect (translate-char c style)
     and unless (eq style :positional)
     collect (get-power power style)))


;; cl:format interface
(defparameter *digits-after-dot* 2)

(defun pprint-jp-numeral (stream object &optional colon-p at-sign-p
			  (digits-after-dot *digits-after-dot*) (scale 0)
			  decimal-mark)
  (unless (numberp object)
    (error "~A is not an expected type for jp-numeral" (type-of object)))
  (unless (= *print-base* 10)
    (error "*print-base* must be 10 for jp-numeral"))
  (let ((style (cond ((and colon-p at-sign-p) :positional)
		     (colon-p :formal)
		     (at-sign-p :old)
		     (t :normal)))
	(outputted? nil))
    (flet ((write-string-list (str-list)
	     (when str-list
	       (setf outputted? t)
	       (mapc #'(lambda (s) (write-string s stream)) str-list))))
      (when (minusp object)
	(write-string (get-minus-sign style) stream))
      (ctypecase object
	(integer
	 ;; TODO: catch overflow-error, and use alternative string.
	 (let ((strs (make-integer-string (abs object) style)))
	   (write-string-list strs)))
	(ratio
	 ;; (assert (plusp (denominator object))) ; Hyperspec 12.1.3.2
	 (let ((numerator-strs (make-integer-string (abs (numerator object)) style))
	       (denominator-strs (make-integer-string (denominator object) style))
	       (parts-of-str (get-parts-of style)))
	   (cond ((eq style :positional)
		  (write-string-list numerator-strs)
		  (write-string parts-of-str stream)
		  (write-string-list denominator-strs))
		 (t
		  ;; sign is printed as a mixed-fraction style.
		  (write-string-list denominator-strs)
		  (write-string parts-of-str stream)
		  (write-string-list numerator-strs)))))
	(float
	 (let ((int-strs (make-integer-string (abs (truncate object)) style))
	       (float-strs (make-fractional-part-string object style digits-after-dot scale)))
	   (write-string-list int-strs)
	   ;; FIXME: should print "〇割一分" ?
	   (when outputted?
	     (if decimal-mark 
		 (write-char decimal-mark stream)
		 (write-string (get-radix-point style) stream)))
	   (write-string-list float-strs))))
      (unless outputted?
	(write-string (get-digit 0 style) stream)))))


;; TODO: rewrite with appropriate args.
(setf (fdefinition 'j)
      #'pprint-jp-numeral)
