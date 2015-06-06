(in-package :jp-numeral.test)

(defmacro assert-progn (&body forms)
  `(progn ,@(loop for i in forms
	       collect `(assert ,i))
	  t))
