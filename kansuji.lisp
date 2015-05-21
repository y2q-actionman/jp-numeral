(in-package :cl-user)

(ql:quickload :babel)
(ql:quickload :alexandria)


(defpackage kansuji
  (:use :cl :babel))

(in-package :kansuji)

(load "kansuji-table.lisp")

(defvar *force-unicode-bmp* nil)

(defun lookup-kansuji (entry usage-sym)
  (alexandria:if-let 
      ((alternative (and *force-unicode-bmp*
			 (aref entry +KANSUJI-TABLE-ALTERNATIVE-IN-BMP-INDEX+))))
    alternative
    (aref entry 
	  (ecase usage-sym
	    ((:normal :positional) +KANSUJI-TABLE-NORMAL-INDEX+)
	    (:financial +KANSUJI-TABLE-FINANCIAL-INDEX+)
	    (:old +KANSUJI-TABLE-OLD-INDEX+)))))

(defun get-kansuji-decimal (n usage-sym)
  (assert (<= 0 n 9))
  (let ((entry (aref *kansuji-decimal-vector* n)))
    (lookup-kansuji entry usage-sym)))
     
(defun get-kansuji-power (n usage-sym)
  (let ((a-entry (assoc n *kansuji-power-alist*)))
    (assert a-entry)
    (lookup-kansuji (cdr a-entry) usage-sym)))
     

(defun make-kansuji-digits4-string (digits4 style)
  (assert (<= 0 digits4 9999))
  (let ((fill-1 (ecase style
		  (:normal nil)
		  ((:financial :old) t)))
	(buf (make-array '(10) :element-type 'character :fill-pointer 0)))
    (labels ((put-kansuji (str)
	       (assert (= (length str) 1))
	       (vector-push (char str 0) buf))
	     (put-hundred-thousand (digit pow)
	       (unless (zerop digit)
		 (when (or (<= 2 digit 9)
			   fill-1)
		   (put-kansuji (get-kansuji-decimal digit style)))
		 (put-kansuji (get-kansuji-power pow style)))))
      (multiple-value-bind (d3 d3-rest) (floor digits4 1000)
	(put-hundred-thousand d3 3)
	(multiple-value-bind (d2 d2-rest) (floor d3-rest 100)
	  (put-hundred-thousand d2 2)
	  ;; 0 - 99
	  (multiple-value-bind (d1 d0) (floor d2-rest 10)
	    (unless (zerop d1)
	      (when (<= 2 d1 9)	       ; fill-1 is not respected here.
		(put-kansuji (get-kansuji-decimal d1 style)))
	      (put-kansuji (get-kansuji-power 1 style)))
	    (unless (zerop d0)
	      (put-kansuji (get-kansuji-decimal d0 style)))))))
    buf))

(defun make-kansuji-integer-string (object style)
  (declare (type integer object))
  (let ((strs nil))
    (loop for power from 0 by 4
       for (rest digits4) = (multiple-value-list (floor object 10000))
       then (multiple-value-list (floor rest 10000))
       as digits4-str = (make-kansuji-digits4-string digits4 style)
       ;; TODO: check 10^68 -- power-str may be null
       as power-str = (get-kansuji-power power style)
       when (plusp (length digits4-str))
       do (push power-str strs)
	 (push digits4-str strs)
       while (> rest 0))
    strs))

(defun make-positional-kansuji-integer-string (object)
  (let ((strs nil))
    (loop for power from 0
       for (rest digit) = (multiple-value-list (floor object 10))
       then (multiple-value-list (floor rest 10))
       as kansuji = (get-kansuji-decimal digit :positional)
       do (push kansuji strs)
       while (> rest 0))
    strs))

(defun print-kansuji (stream object &optional colon-p at-sign-p)
  (let ((style (cond ((and colon-p at-sign-p) :positional)
		     (colon-p :financial)
		     (at-sign-p :old)
		     (t :normal))))
    (ctypecase object
      (integer
       (let ((kansuji-strs 
	      (if (eq style :positional)
		  (make-positional-kansuji-integer-string object)
		  (make-kansuji-integer-string object style))))
	 (loop for s in kansuji-strs
	    do (write-string s stream)))))))
