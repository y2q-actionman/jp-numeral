(in-package :jp-numeral.test)

(define-modify-macro mulf (&rest args) *)


(defun style-to-flag (style)
  (ecase style
    (:positional (values t t))
    (:formal (values t nil))
    (:old (values nil t))
    (:normal (values nil nil))))

(defmacro define-jp-stringify (name lambda-list function)
  (let ((style-sym (first lambda-list))
	(num-sym (second lambda-list))
	(keyargs (rest (member '&key lambda-list)))) ; too ad-hoc..
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


(defun equal-or-free-format (style expect number)
  (let ((actual (jp-str style number))
	(free-format (format nil "~F" number)))
    (or (equal expect actual)
	(equal free-format actual))))
