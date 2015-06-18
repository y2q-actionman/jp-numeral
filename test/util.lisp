(in-package :jp-numeral.test)

(define-modify-macro mulf (&rest args) *)

(defun assert-equal (a b)
  (assert (equal a b)
	  (a b)
	  "Test assertion failure: (equal ~A ~A), value" a b))

(defun assert-equal-or-free-format (style expect number)
  (let ((actual (jp-str style number))
	(free-format (format nil "~F" number)))
    (assert (or (equal expect actual)
		(equal free-format actual))
	    ()
	    "Test assertion failure: (or (equal ~A ~A) (equal ~A ~1@*~A))"
	    expect actual free-format)))


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
