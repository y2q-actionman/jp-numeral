(in-package :jp-numeral)

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
  (alexandria:if-let ((a-entry (assoc n +power-alist+)))
    (aref (cdr a-entry)
	  (style-to-index style nil))))

(defun get-sign (style)
  (aref +minus-sign+
	(style-to-index style t)))
  
(defun get-parts-of (style)
  (aref +fraction-parts-of+
	(style-to-index style t)))
     

(defun make-digits4-string (digits4 style)
  (declare (type integer digits4))
  (assert (<= 0 digits4 9999))
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

(define-condition overflow-error (error)
  ())

(defun make-positional-integer-string (object)
  (declare (type integer object))
  (let ((strs nil))
    (loop for power from 0
       for (rest digit) = (multiple-value-list (floor (abs object) 10))
       then (multiple-value-list (floor rest 10))
       do (push (get-digit digit :positional) strs)
       while (> rest 0))
    (when (minusp object)
      (push (get-sign :positional) strs))
    strs))

(defun make-integer-string (object style)
  (declare (type integer object))
  (when (eq style :positional)
    (return-from make-integer-string
      (make-positional-integer-string object)))
  (cond
    ((zerop object)
     (list (get-digit 0 style)))
    ((>= object (expt 10 (+ +power-max+ 4)))
     (error 'overflow-error))
    (t
     (let ((strs nil))
       (loop for power from 0 by 4
	  for (rest digits4) = (multiple-value-list (floor (abs object) 10000))
	  then (multiple-value-list (floor rest 10000))
	  as digits4-str = (make-digits4-string digits4 style)
	  as power-str = (get-power power style)
	  when (plusp (length digits4-str))
	  do (push power-str strs)
	    (push digits4-str strs)
	  while (> rest 0))
       (when (minusp object)
	 (push (get-sign style) strs))
       strs))))

;; cl:format interface
(defun pprint-jp-numeral (stream object &optional colon-p at-sign-p)
  (unless (numberp object)
    (error "~A is not an expected type for jp-numeral" (type-of object)))
  (unless (= *print-base* 10)
    (error "*print-base* must be 10 for jp-numeral"))
  (let ((style (cond ((and colon-p at-sign-p) :positional)
		     (colon-p :formal)
		     (at-sign-p :old)
		     (t :normal))))
    (flet ((write-string-list (str-list)
	     (mapc #'(lambda (s) (write-string s stream)) str-list)))
      (ctypecase object
	(integer
	 ;; TODO: catch overflow-error, and use alternative string.
	 (let ((strs (make-integer-string object style)))
	   (write-string-list strs)))
	(ratio
	 (let ((numerator-strs (make-integer-string (abs (numerator object)) style))
	       (numerator-sign-str (if (minusp (numerator object))
				       (get-sign style) ""))
	       (denominator-strs (make-integer-string (denominator object) style))
	       (parts-of-str (get-parts-of style)))
	   ;; (assert (plusp (denominator object))) ; Hyperspec 12.1.3.2
	   (cond ((eq style :positional)
		  (write-string numerator-sign-str stream)
		  (write-string-list numerator-strs)
		  (write-string parts-of-str stream)
		  (write-string-list denominator-strs))
		 (t
		  ;; sign is printed as a mixed-fraction style.
		  (write-string numerator-sign-str stream)
		  (write-string-list denominator-strs)
		  (write-string parts-of-str stream)
		  (write-string-list numerator-strs)))))))))

;; TODO: rewrite with appropriate args.
(setf (fdefinition 'p)
      #'pprint-jp-numeral)
