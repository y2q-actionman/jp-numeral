(in-package :jp-numeral.test)

(define-modify-macro mulf (&rest args) *)

(defmacro assert-equal (form-a form-b)
  (let ((val-a (gensym))
	(val-b (gensym)))
    `(let ((,val-a ,form-a)
	  (,val-b ,form-b))
       (assert (equal ,val-a ,val-b)
	       nil
	       "Test assertion failure~_~S~_~A~_~A"
	       '(equal ,form-a ,form-b) ,val-a ,val-b))))

(defun assert-equal-or-free-format (style expect number)
  (let ((actual (jp-str style number))
	(free-format (format nil "~F" number)))
    (assert (or (equal expect actual)
		(equal free-format actual))
	    ()
	    "Test assertion failure: (or (equal ~A ~A) (equal ~A ~1@*~A))"
	    expect actual free-format)))

(defmacro assert-some-condition (&body body)
  (let ((returns (gensym)))
    `(destructuring-bind (&rest ,returns)
	 (multiple-value-list (ignore-errors ,@body))
       (unless (and (null (first ,returns))
		    (second ,returns))
	 (assert nil nil
		 "This form should cause some condition: ~A" ',body)))))


(defun style-to-flag (style)
  (ecase style
    (:positional (values t t))
    (:formal (values t nil))
    (:old (values nil t))
    (:normal (values nil nil))))

(defmacro define-jp-stringify (name lambda-list function)
  (let ((style-sym (first lambda-list))
	(num-sym (second lambda-list))
	(keyargs (loop for i on lambda-list
		    when (eq (car i) '&key)
		    return (cdr i)
		    finally (return nil))))
    `(defun ,name (,style-sym ,num-sym &key ,@keyargs)
       (multiple-value-bind (colon-p at-sign-p)
	   (style-to-flag ,style-sym)
	 (with-output-to-string (stream)
	   (,function stream ,num-sym colon-p at-sign-p
		      ,@keyargs))))))

(define-jp-stringify jp-str
    (style num &key digits-after-dot scale radix-point)
  jp-numeral:jp)

(define-jp-stringify wari-str
    (style num &key digits-after-dot)
  jp-numeral:wari)

(define-jp-stringify yen-str
    (style num &key digits-after-dot)
  jp-numeral:yen)
