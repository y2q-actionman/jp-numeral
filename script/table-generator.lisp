;;; -*- coding: utf-8; -*-

(in-package :cl-user)

(defpackage jp-numeral.table-generator
  (:use :cl)
  (:import-from :alexandria
		:define-constant)
  (:export #:generate-file))

(in-package :jp-numeral.table-generator)


(defclass octets-printer ()
  ((octets :initarg :octets :accessor octets-printer-octets)))

(defmethod print-object ((obj octets-printer) stream)
  (format stream "#.~S"
	  `(babel:octets-to-string
	    (make-array ',(array-dimensions (octets-printer-octets obj))
			:initial-contents ',(coerce (octets-printer-octets obj) 'list)
			:element-type '(unsigned-byte 8))
	    :encoding :utf-8)))
  
(defun to-octets-printer (str)
  (if str
      (make-instance 'octets-printer
		     :octets (babel:string-to-octets str :encoding :utf-8))
      nil))


(define-constant +digits+
    #(("〇" nil "零")
      ("一" "壱" "壹")
      ("二" "弐" "貳")
      ("三" "参" "參")
      ("四" nil "肆")
      ("五" nil "伍")
      ("六" nil "陸")
      ("七" nil "柒")
      ("八" nil "捌")
      ("九" nil "玖"))
  :test 'equalp)

(defun make-digits-load-form ()
  (loop with buf = (make-array (array-dimensions +digits+)
			       :fill-pointer 0)
     for (normal formal old) across +digits+
     as normal-octets = (to-octets-printer normal)
     as formal-octets = (or (to-octets-printer formal) normal-octets)
     as old-octets = (or (to-octets-printer old) formal-octets)
     do (vector-push (vector normal-octets formal-octets old-octets) buf)
     finally (return buf)))


(define-constant +power-alist+
    '((1 . ("十" "拾" nil))
      (2 . ("百" nil "佰"))
      (3 . ("千" nil "仟"))
      ;; myriads
      (4 . ("万" nil "萬"))
      (8 . "億")
      (12 . "兆")
      (16 . "京")
      (20 . "垓")
      (24 . (#(240 165 157 177) nil "秭")) ; U+25771 may be out of Lisp string..
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
      (-21 . "清浄"))
  :test 'equalp)

(defun make-power-alist-load-form ()
  (loop for (i . data) in +power-alist+
     collect
     (etypecase data
       (string
	(let ((octets (to-octets-printer data)))
	  (cons i (vector octets octets octets))))
       (list
	(destructuring-bind (normal formal old) data
	  (let* ((normal-octets (etypecase normal
				  (string
				   (to-octets-printer normal))
				  (vector
				   (make-instance 'octets-printer
						  :octets normal))))
		 (formal-octets (or (to-octets-printer formal) normal-octets))
		 (old-octets (or (to-octets-printer old) formal-octets)))
	    (cons i (vector normal-octets formal-octets old-octets))))))))


(define-constant +power-max+
    (apply #'max (mapcar #'car +power-alist+)))

(define-constant +power-min+
    (apply #'min (mapcar #'car +power-alist+)))


(defun make-string-array-load-form (arr)
  (loop with buf = (make-array (array-dimensions arr)
			       :fill-pointer 0)
     as prev-octets = nil then (or str-octets prev-octets)
     for str across arr
     as str-octets = (or (to-octets-printer str) prev-octets)
     do (vector-push str-octets buf)
     finally (return buf)))


(define-constant +minus-sign+
    #("マイナス" "負の" "負之" "−")
  :test 'equalp)

(define-constant +fraction-parts-of+
    #("分の" nil "分之" "／")
  :test 'equalp)

(define-constant +radix-point+
    #("・" nil nil nil)
  :test 'equalp)

(define-constant +sen+
    #("銭" nil "錢")
  :test 'equalp)

(define-constant +yen+
    #("円" nil "圓")
  :test 'equalp)

(define-constant +wari+
    "割"
  :test 'equalp)


(defun generate-file (output-file &optional (*package* *package*))
  (with-open-file (stream output-file
			  :direction :output :if-exists :rename
			  :if-does-not-exist :create)
    (labels ((gen-output-symbol (sym)
	       (intern (symbol-name sym) *package*))
	     (make-const-form (sym val &optional docstring)
	       `(define-constant ,(gen-output-symbol sym) ,val
		  :test 'equalp :documentation ,docstring))
	     (make-str-array-form (sym)
	       (make-const-form sym (make-string-array-load-form (symbol-value sym))
		   "A vector of (<normal> <formal> <old> <positional>")))
      (let ((*print-circle* t)
	    ;; (*print-pretty* nil) ; currently commented out for debug. TODO: enable this.
	    )
	(format stream "~S~%"
		`(in-package ,(package-name *package*)))
	(terpri stream)
	(format stream "~S~%"
		(make-const-form '+table-normal-index+ 0))
	(format stream "~S~%"
		(make-const-form '+table-formal-index+ 1))
	(format stream "~S~%"
		(make-const-form '+table-old-index+ 2))
	(format stream "~S~%"
		(make-const-form '+table-positional-index+ 3))
	(terpri stream)
	(format stream "~S~%"
		`(define-constant ,(gen-output-symbol '+digits+)
		     ',(make-digits-load-form)
		   :test 'equalp
		   :documentation "A vector of (<normal> <formal> <old>)"))
	(terpri stream)
	(format stream "~S~%"
		`(define-constant ,(gen-output-symbol '+power-alist+)
		     ',(make-power-alist-load-form)
		   :test 'equalp
		   :documentation "An alist of (<power> . (<normal> <formal> <old>))"))
	(terpri stream)
	(format stream "~S~%"
		(make-const-form '+power-max+ +power-max+))
	(terpri stream)
	(format stream "~S~%"
		(make-const-form '+power-min+ +power-min+))
	(terpri stream)
	(format stream "~S~%"
		(make-str-array-form '+minus-sign+))
	(terpri stream)
	(format stream "~S~%"
		(make-str-array-form '+fraction-parts-of+))
	(terpri stream)
	(format stream "~S~%"
		(make-str-array-form '+radix-point+))
	(terpri stream)
	(format stream "~S~%"
		(make-str-array-form '+yen+))
	(terpri stream)
	(format stream "~S~%"
		(make-str-array-form '+sen+))
	(terpri stream)
	(format stream "~S~%"
		(make-const-form '+wari+ (to-octets-printer +wari+)))
	))))
