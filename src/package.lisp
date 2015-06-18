(in-package :cl-user)

(defpackage jp-numeral
  (:use :cl)
  (:import-from :alexandria
		#:define-constant)
  (:export
   #:format-jp-numeral
   #:jp
   #:wari
   #:yen
   ))
