(in-package :jp-numeral.test)

(defun main ()
  (and (test-jp-normal)
       (test-jp-formal)
       (test-jp-old)
       (test-jp-positional)
       (test-wari)
       ;; TODO: yen
       t))
