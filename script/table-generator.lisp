;;; -*- coding: utf-8; -*-

(in-package :cl-user)

(defpackage jp-numeral.table-generator
  (:use :cl)
  (:import-from :alexandria
		#:define-constant
		#:alist-hash-table)
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
  
(defun to-table-entry-load-form (entry)
  (flet ((to-octets-printer (obj)
	   (etypecase obj
	     (string
	      (make-instance 'octets-printer
			     :octets (babel:string-to-octets obj :encoding :utf-8)))
	     (vector
	      (make-instance 'octets-printer
			     :octets obj)))))
    (destructuring-bind (normal &optional formal old positional) entry
      (let* ((normal-octets (to-octets-printer normal))
	     (formal-octets (if formal
				(to-octets-printer formal)
				normal-octets))
	     (old-octets (if old
			     (to-octets-printer old)
			     formal-octets))
	     (positional-octets (if positional
				    (to-octets-printer positional)
				    normal-octets)))
	(vector normal-octets formal-octets
		old-octets positional-octets)))))


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
     for entry across +digits+
     do (vector-push (to-table-entry-load-form entry) buf)
     finally (return buf)))


(define-constant +power-alist+
    '((0 . (""))
      (1 . ("十" "拾" nil))
      (2 . ("百" nil "佰"))
      (3 . ("千" nil "仟"))
      ;; myriads
      (4 . ("万" nil "萬"))
      (8 . ("億"))
      (12 . ("兆"))
      (16 . ("京"))
      (20 . ("垓"))
      (24 . (#(240 165 157 177) nil "秭")) ; U+25771 (𥝱) may be out of Lisp string..
      ;; BUG: babel on ACL makes #(237 161 149 237 189 177) from U+25771 !!
      (28 . ("穣"))
      (32 . ("溝"))
      (36 . ("澗"))
      (40 . ("正"))
      (44 . ("載"))
      (48 . ("極"))
      (52 . ("恒河沙"))
      (56 . ("阿僧祇"))
      (60 . ("那由他"))
      (64 . ("不可思議"))
      (68 . ("無量大数"))
      ;; fractions
      (-1 . ("分"))
      (-2 . ("厘" nil "釐"))
      (-3 . ("毛" nil "毫"))
      (-4 . ("糸" nil "絲"))
      (-5 . ("忽"))
      (-6 . ("微"))
      (-7 . ("繊"))
      (-8 . ("沙"))
      (-9 . ("塵"))
      (-10 . ("埃"))
      (-11 . ("渺"))
      (-12 . ("漠"))
      (-13 . ("模糊"))
      (-14 . ("逡巡"))
      (-15 . ("須臾"))
      (-16 . ("瞬息"))
      (-17 . ("弾指"))
      (-18 . ("刹那"))
      (-19 . ("六徳"))
      (-20 . ("虚空"))
      (-21 . ("清浄")))
  :test 'equalp)

(defun make-power-alist-load-form ()
  (loop for (i . entry) in +power-alist+
     collect (cons i (to-table-entry-load-form entry))))


(define-constant +minus-sign+
    '("マイナス" nil "負之" "−")
  :test 'equalp)

(define-constant +fraction-parts-of+
    '("分の" nil "分之" "／")
  :test 'equalp)

(define-constant +radix-point+
    '("・")
  :test 'equalp)

(define-constant +sen+
    '("銭" nil "錢")
  :test 'equalp)

(define-constant +yen+
    '("円" nil "圓")
  :test 'equalp)

(define-constant +wari+
    '("割")
  :test 'equalp)

(defun make-string-array-load-form (str-lis)
  (to-table-entry-load-form str-lis))


(defun generate-file (output-file &optional (output-package *package*))
  (with-open-file (stream output-file
			  :direction :output :if-exists :rename
			  :if-does-not-exist :create)
    (labels ((gen-output-symbol (sym)
	       (intern (symbol-name sym) output-package))
	     (make-const-form (sym val &optional docstring)
	       `(define-constant ,(gen-output-symbol sym) ,val
		  :test 'equalp :documentation ,docstring))
	     (make-const-str-array-form (sym)
	       (make-const-form sym (make-string-array-load-form (symbol-value sym))
				"A vector of (<normal> <formal> <old> <positional>")))
      (with-standard-io-syntax
	(let ((*print-circle* t)
	      (*package* output-package))
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
		  (make-const-form '+digits+ `',(make-digits-load-form)
				   "A vector of (<normal> <formal> <old> <positional>)"))
	  (terpri stream)
	  (format stream "~S~%"
		  (make-const-form '+power-hash-table+
				   `(alist-hash-table ',(make-power-alist-load-form))
				   "An alist of (<power> . (<normal> <formal> <old> <positional>))"))
	  (terpri stream)
	  (format stream "~S~%"
		  (make-const-str-array-form '+minus-sign+))
	  (terpri stream)
	  (format stream "~S~%"
		  (make-const-str-array-form '+fraction-parts-of+))
	  (terpri stream)
	  (format stream "~S~%"
		  (make-const-str-array-form '+radix-point+))
	  (terpri stream)
	  (format stream "~S~%"
		  (make-const-str-array-form '+yen+))
	  (terpri stream)
	  (format stream "~S~%"
		  (make-const-str-array-form '+sen+))
	  (terpri stream)
	  (format stream "~S~%"
		  (make-const-str-array-form '+wari+))
	  (fresh-line stream))))))
