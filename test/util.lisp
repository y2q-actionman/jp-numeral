(in-package :jp-numeral.test)

(define-modify-macro mulf (&rest args) *)

(defun style-to-flag (style)
  (ecase style
    (:positional (values t t))
    (:formal (values t nil))
    (:old (values nil t))
    (:normal (values nil nil))))

(defun jpn (num style
	    &key digits-after-dot scale radix-point)
  (multiple-value-bind (colon-p at-sign-p)
      (style-to-flag style)
    (with-output-to-string (stream)
      (jp-numeral:jp stream num colon-p at-sign-p
		     digits-after-dot scale radix-point))))
