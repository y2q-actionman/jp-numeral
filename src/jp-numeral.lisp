(in-package :jp-numeral)

(defun get-jp-numeral-from-entry (entry usage-sym)
  (aref entry 
	(ecase usage-sym
	  ((:normal :positional) +JP-NUMERAL-TABLE-NORMAL-INDEX+)
	  (:financial +JP-NUMERAL-TABLE-FINANCIAL-INDEX+)
	  (:old +JP-NUMERAL-TABLE-OLD-INDEX+))))

(defun get-jp-numeral-decimal (n usage-sym)
  (assert (<= 0 n 9))
  (let ((a-entry (assoc n +jp-numeral-decimal-alist+)))
    (assert a-entry)
    (get-jp-numeral-from-entry (cdr a-entry) usage-sym)))
     
(defun get-jp-numeral-power (n usage-sym)
  (alexandria:if-let ((a-entry (assoc n +jp-numeral-power-alist+)))
    (get-jp-numeral-from-entry (cdr a-entry) usage-sym)))

;; TODO: remove
(defun get-jp-numeral-from-list (usage-sym list)
  (nth (ecase usage-sym
	 (:normal +JP-NUMERAL-TABLE-NORMAL-INDEX+)
	 (:financial +JP-NUMERAL-TABLE-FINANCIAL-INDEX+)
	 (:old +JP-NUMERAL-TABLE-OLD-INDEX+)
	 (:positional +JP-NUMERAL-TABLE-POSITIONAL-INDEX+))
       list))
  
(defun get-jp-numeral-sign (usage-sym)
  (get-jp-numeral-from-list usage-sym
			    +jp-numeral-sign-list+))
  
(defun get-jp-numeral-parts-of (usage-sym)
  (get-jp-numeral-from-list usage-sym
			    +JP-NUMERAL-FRACTION-PARTS-OF-LIST+))
     

(defun make-jp-numeral-digits4-string (digits4 style)
  (assert (<= 0 digits4 9999))
  (let ((fill-1 (ecase style
		  (:normal nil)
		  ((:financial :old) t)))
	(buf (make-array '(10) :element-type 'character :fill-pointer 0)))
    (labels ((put-jp-numeral (str)
	       (assert (= (length str) 1))
	       (vector-push (char str 0) buf))
	     (put-hundred-thousand (digit pow)
	       (unless (zerop digit)
		 (when (or (<= 2 digit 9)
			   fill-1)
		   (put-jp-numeral (get-jp-numeral-decimal digit style)))
		 (put-jp-numeral (get-jp-numeral-power pow style)))))
      (multiple-value-bind (d3 d3-rest) (floor digits4 1000)
	(put-hundred-thousand d3 3)
	(multiple-value-bind (d2 d2-rest) (floor d3-rest 100)
	  (put-hundred-thousand d2 2)
	  ;; 0 - 99
	  (multiple-value-bind (d1 d0) (floor d2-rest 10)
	    (unless (zerop d1)
	      (when (<= 2 d1 9)	       ; fill-1 is not respected here.
		(put-jp-numeral (get-jp-numeral-decimal d1 style)))
	      (put-jp-numeral (get-jp-numeral-power 1 style)))
	    (unless (zerop d0)
	      (put-jp-numeral (get-jp-numeral-decimal d0 style)))))))
    buf))

(define-condition jp-numeral-overflow-error (error)
  ())

(defun make-positional-jp-numeral-integer-string (object)
  (declare (type integer object))
  (let ((strs nil))
    (loop for power from 0
       for (rest digit) = (multiple-value-list (floor (abs object) 10))
       then (multiple-value-list (floor rest 10))
       as jp-numeral = (get-jp-numeral-decimal digit :positional)
       do (push jp-numeral strs)
       while (> rest 0))
    (when (minusp object)
      (push (get-jp-numeral-sign :positional) strs))
    strs))

(defun make-jp-numeral-integer-string (object style)
  (declare (type integer object))
  (when (eq style :positional)
    (return-from make-jp-numeral-integer-string
      (make-positional-jp-numeral-integer-string object)))
  (cond
    ((zerop object)
     (list (get-jp-numeral-decimal 0 style)))
    ((>= object (expt 10 (+ +jp-numeral-power-max+ 4)))
     (error 'jp-numeral-overflow-error))
    (t
     (let ((strs nil))
       (loop for power from 0 by 4
	  for (rest digits4) = (multiple-value-list (floor (abs object) 10000))
	  then (multiple-value-list (floor rest 10000))
	  as digits4-str = (make-jp-numeral-digits4-string digits4 style)
	  as power-str = (get-jp-numeral-power power style)
	  when (plusp (length digits4-str))
	  do (push power-str strs)
	    (push digits4-str strs)
	  while (> rest 0))
       (when (minusp object)
	 (push (get-jp-numeral-sign style) strs))
       strs))))

(defun %pprint-jp-numeral (stream object &optional (style :normal))
  (unless (numberp object)
    (error "~A is not an expected type for jp-numeral" (type-of object)))
  (flet ((write-string-list (str-list)
	   (mapc #'(lambda (s) (write-string s stream)) str-list)))
    (ctypecase object
      (integer
       ;; TODO: catch jp-numeral-overflow-error, and use alternative string.
       (let ((jp-numeral-strs (make-jp-numeral-integer-string object style)))
	 (write-string-list jp-numeral-strs)))
      (ratio
       (let ((numerator-strs (make-jp-numeral-integer-string (abs (numerator object)) style))
	     (numerator-sign-str (if (minusp (numerator object))
				     (get-jp-numeral-sign style) ""))
	     (denominator-strs (make-jp-numeral-integer-string (denominator object) style))
	     (parts-of-str (get-jp-numeral-parts-of style)))
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
		(write-string-list numerator-strs))))))))

;; cl:format interface
(defun pprint-jp-numeral (stream object &optional colon-p at-sign-p)
  (let ((style (cond ((and colon-p at-sign-p) :positional)
		     (colon-p :financial)
		     (at-sign-p :old)
		     (t :normal))))
    (%pprint-jp-numeral stream object style)))
