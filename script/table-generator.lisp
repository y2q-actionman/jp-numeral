;;; -*- coding: utf-8; -*-

(in-package :cl-user)

(defpackage jp-numeral.table-generator
  (:use :cl)
  (:export #:generate-file))

(in-package :jp-numeral.table-generator)


(defclass octets-printer ()
  ((octets :initarg :octets :accessor octets-printer-octets)))

(defmethod print-object ((obj octets-printer) stream)
  (format stream "#.~S"
	  `(babel:octets-to-string
	    (make-array ',(array-dimensions (octets-printer-octets obj))
			:element-type '(unsigned-byte 8)
			:initial-contents ',(coerce (octets-printer-octets obj) 'list))
	    :encoding :utf-8)))
  
(defun to-octets-printer (str default)
  (if str
      (make-instance 'octets-printer
		     :octets (babel:string-to-octets str :encoding :utf-8))
      default))


(defconstant +jp-numeral-decimal-alist+
  '((0 . ("〇" nil "零"))
    (1 . ("一" "壱" "壹"))
    (2 . ("二" "弐" "貳"))
    (3 . ("三" "参" "參"))
    (4 . ("四" nil "肆"))
    (5 . ("五" nil "伍"))
    (6 . ("六" nil "陸"))
    (7 . ("七" nil "柒"))
    (8 . ("八" nil "捌"))
    (9 . ("九" nil "玖"))))

(defun make-jp-numeral-decimal-alist-load-form ()
  (loop with ret = nil
     for (num . (normal financial old)) in +jp-numeral-decimal-alist+
     as normal-octets = (to-octets-printer normal nil)
     as financial-octets = (to-octets-printer financial normal-octets)
     as old-octets = (to-octets-printer old financial-octets)
     do (push (cons num (vector normal-octets financial-octets old-octets nil))
	      ret)
     finally (return (nreverse ret))))


(defconstant +jp-numeral-power-alist+
  '((0 . ("" nil nil))
    (1 . ("十" "拾" nil))
    (2 . ("百" nil "佰"))
    (3 . ("千" nil "仟"))
    ;; myriads
    (4 . ("万" nil "萬"))
    (8 . "億")
    (12 . "兆")
    (16 . "京")
    (20 . "垓")
    (24 . (#(240 165 157 177) nil "秭" "秭")) ; U+25771 may be out of Lisp string..
    ;; BUG: babel on ACL makes #(237 161 149 237 189 177) from U+25771 !!
    (28 . "穣")
    (32 . "溝")
    (36 . "澗")
    (40 . "正")
    (44 . "載")
    (48 . "極")
    (52 . "恒河沙")
    (56 . "阿僧祇")
    (60 . "那由他")
    (64 . "不可思議")
    (68 . "無量大数")
    ;; fractions
    (-1 . "分")
    (-2 . ("厘" nil "釐"))
    (-3 . ("毛" nil "毫"))
    (-4 . ("糸" nil "絲"))
    (-5 . "忽")
    (-6 . "微")
    (-7 . "繊")
    (-8 . "沙")
    (-9 . "塵")
    (-10 . "埃")
    (-11 . "渺")
    (-12 . "漠")
    (-13 . "模糊")
    (-14 . "逡巡")
    (-15 . "須臾")
    (-16 . "瞬息")
    (-17 . "弾指")
    (-18 . "刹那")
    (-19 . "六徳")
    (-20 . "虚空")
    (-21 . "清浄")))

(defun make-jp-numeral-power-alist-load-form ()
  (loop with ret = nil
     for (i . data) in +jp-numeral-power-alist+
     do (etypecase data
	  (string
	   (let ((octets (to-octets-printer data nil)))
	     (push (cons i (vector octets octets octets nil))
		   ret)))
	  (list
	   (destructuring-bind (normal financial old &optional unicode-bmp-alternative) data
	     (let* ((normal-octets (etypecase normal
				     (string
				      (to-octets-printer normal nil))
				     (vector
				      (make-instance 'octets-printer
						     :octets normal))))
		    (financial-octets (to-octets-printer financial normal-octets))
		    (old-octets (to-octets-printer old financial-octets))
		    (alt-octets (to-octets-printer unicode-bmp-alternative nil)))
	       (push (cons i (vector normal-octets financial-octets old-octets
				     alt-octets))
		     ret)))))
     finally (return (nreverse ret))))


(defconstant +jp-numeral-power-max+
  (apply #'max (mapcar #'car +jp-numeral-power-alist+)))


(defconstant +jp-numeral-sign-list+
  '("マイナス" "負の" "負之" "−"))

(defun make-jp-numeral-sign-list-load-form ()
  (loop for str in +jp-numeral-sign-list+
     collect (to-octets-printer str nil)))


(defconstant +jp-numeral-fraction-parts-of-list+
  '("分の" nil "分之" "／"))

(defun make-jp-numeral-fraction-parts-of-list-load-form ()
  (loop as prev-octets = nil then (or str-octets prev-octets)
     for str in +jp-numeral-fraction-parts-of-list+
     as str-octets = (to-octets-printer str prev-octets)
     collect str-octets))


(defun generate-file (output-file &optional (*package* *package*))
  (with-open-file (stream output-file
			  :direction :output :if-exists :rename
			  :if-does-not-exist :create)
    (flet ((gen-output-symbol (sym)
	     (intern (symbol-name sym) *package*)))
      (let ((*print-circle* t))
	(format stream "~S~%"
		`(in-package ,(package-name *package*)))
	(terpri stream)
	(format stream "~S~%"
		`(defconstant ,(gen-output-symbol '+jp-numeral-table-normal-index+) 0))
	(format stream "~S~%"
		`(defconstant ,(gen-output-symbol '+jp-numeral-table-financial-index+) 1))
	(format stream "~S~%"
		`(defconstant ,(gen-output-symbol '+jp-numeral-table-old-index+) 2))
	(format stream "~S~%"
		`(defconstant ,(gen-output-symbol '+jp-numeral-table-alternative-in-bmp-index+) 3))
	(terpri stream)
	(format stream "~S~%"
		`(defconstant ,(gen-output-symbol '+jp-numeral-decimal-alist+)
		   ',(make-jp-numeral-decimal-alist-load-form)
		   "A vector of (<normal> <financial> <old> <alternative-in-BMP-of-Unicode>)"))
	(terpri stream)
	(format stream "~S~%"
		`(defconstant ,(gen-output-symbol '+jp-numeral-power-alist+)
		   ',(make-jp-numeral-power-alist-load-form)
		   "An alist of (<power> . (<normal> <financial> <old> <alternative-in-BMP-of-Unicode>))"))
	(terpri stream)
	(format stream "~S~%"
		`(defconstant ,(gen-output-symbol '+jp-numeral-power-max+)
		   ,+jp-numeral-power-max+))
	(terpri stream)
	(format stream "~S~%"
		`(defconstant ,(gen-output-symbol '+jp-numeral-sign-list-positional-index+) 3))
	(terpri stream)
	(format stream "~S~%"
		`(defconstant ,(gen-output-symbol '+jp-numeral-sign-list+)
		   ',(make-jp-numeral-sign-list-load-form)
		   "A list of (<normal> <financial> <old> <positional>"))
	(terpri stream)
	(format stream "~S~%"
		`(defconstant ,(gen-output-symbol '+jp-numeral-fraction-parts-of-list+)
		   ',(make-jp-numeral-fraction-parts-of-list-load-form)
		   "A list of (<normal> <financial> <old> <positional>"))
	))))
