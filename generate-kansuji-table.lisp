;; -*- coding: utf-8; -*-

(in-package :cl-user)

(ql:quickload :babel)


(defpackage kansuji-table-generator
  (:use :cl :babel)
  (:export #:generate-kansuji-table-file))

(in-package :kansuji-table-generator)


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


(defparameter *kansuji-decimal-vector*
  #(("〇" nil "零")
    ("一" "壱" "壹")
    ("二" "弐" "貳")
    ("三" "参" "參")
    ("四" nil "肆")
    ("五" nil "伍")
    ("六" nil "陸")
    ("七" nil "柒")
    ("八" nil "捌")
    ("九" nil "玖")))

(defun make-kansuji-decimal-vector-load-form ()
  (loop with ret = (make-array (length *kansuji-decimal-vector*)
			       :fill-pointer 0)
     for (normal financial old) across *kansuji-decimal-vector*
     as normal-octets = (to-octets-printer normal nil)
     as financial-octets = (to-octets-printer financial normal-octets)
     as old-octets = (to-octets-printer old financial-octets)
     do (vector-push (vector normal-octets financial-octets old-octets nil)
		     ret)
     finally (return ret)))


(defparameter *kansuji-power-alist*
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

(defun make-kansuji-power-alist-load-form ()
  (loop with ret = nil
     for (i . data) in *kansuji-power-alist*
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


(defun generate-kansuji-table-file (output-file &optional (*package* *package*))
  (with-open-file (stream output-file
			  :direction :output :if-exists :error
			  :if-does-not-exist :create)
    (flet ((gen-output-symbol (sym)
	     (intern (symbol-name sym) *package*)))
      (let ((*print-circle* t))
	(format stream "~S~%"
		`(in-package ,(package-name *package*)))
	(terpri stream)
	(format stream "~S~%"
		`(defconstant ,(gen-output-symbol '+kansuji-table-normal-index+) 0))
	(format stream "~S~%"
		`(defconstant ,(gen-output-symbol '+kansuji-table-financial-index+) 1))
	(format stream "~S~%"
		`(defconstant ,(gen-output-symbol '+kansuji-table-old-index+) 2))
	(format stream "~S~%"
		`(defconstant ,(gen-output-symbol '+kansuji-table-alternative-in-bmp-index+) 2))
	(terpri stream)
	(format stream "~S~%"
		`(defparameter ,(gen-output-symbol '*kansuji-decimal-vector*)
		   ,(make-kansuji-decimal-vector-load-form)
		   "A vector of (<normal-octets> <financial-octets> <old-octets> <alternative-in-BMP-of-Unicode>)"
		   ))
	(terpri stream)
	(format stream "~S~%"
		`(defparameter ,(gen-output-symbol '*kansuji-power-alist*)
		   ',(make-kansuji-power-alist-load-form)
		   "An alist of (<power> . (<normal-octets> <financial-octets> <old-octets> <alternative-in-BMP-of-Unicode>))"))))))
