(in-package :cl-user)

(defpackage jp-numeral
  (:use :cl)
  (:import-from :alexandria
		#:define-constant)
  (:export
   #:pprint-jp-numeral
   #:jp
   #:j
   #:wari
   #:w
   #:yen
   #:y))
