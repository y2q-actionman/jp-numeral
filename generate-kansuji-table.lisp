(in-package :cl-user)

(ql:quickload :babel)


(defpackage kansuji-table
  (:use :cl :babel))

(in-package :kansuji-table)


(defun to-utf8-if-nonnull (str default)
  (if str
      (babel:string-to-octets str :encoding :utf-8)
      default))

(defparameter *kansuji-decimal-string-vector*
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

(defun make-kansuji-decimal-octets-vector ()
  (loop with ret = (make-array (length *kansuji-decimal-string-vector*)
			       :fill-pointer 0)
     for (normal financial old) across *kansuji-decimal-string-vector*
     as normal-octets = (to-utf8-if-nonnull normal nil)
     as financial-octets = (to-utf8-if-nonnull financial normal-octets)
     as old-octets = (to-utf8-if-nonnull old financial-octets)
     do (vector-push (vector normal-octets financial-octets old-octets nil)
		     ret)
     finally (return ret)))


(defparameter *kansuji-power-string-alist*
  '((1 . ("十" "拾" nil))
    (2 . ("百" nil "佰"))
    (3 . ("千" nil "仟"))
    ;; myriads
    (4 . ("万" nil "萬"))
    (8 . "億")
    (12 . "兆")
    (16 . "京")
    (20 . "垓")
    (24 . (#(237 161 149 237 189 177) nil "秭" "秭")) ; U+25771 may be out of Lisp string..
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

(defun make-kansuji-power-octets-alist ()
  (loop with ret = (make-array (length *kansuji-power-string-alist*) :fill-pointer 0)
     for (i . data) in *kansuji-power-string-alist*
     do (etypecase data
	  (string
	   (let ((octets (babel:string-to-octets data :encoding :utf-8)))
	     (vector-push (cons i (vector octets octets octets nil))
			  ret)))
	  (list
	   (destructuring-bind (normal financial old &optional unicode-bmp-alternative) data
	     (let* ((normal-octets (etypecase normal
				     (string
				      (to-utf8-if-nonnull normal nil))
				     (vector
				      normal)))
		    (financial-octets (to-utf8-if-nonnull financial normal-octets))
		    (old-octets (to-utf8-if-nonnull old financial-octets))
		    (alt-octets (to-utf8-if-nonnull unicode-bmp-alternative nil)))
	       (vector-push (cons i (vector normal-octets financial-octets old-octets
					    alt-octets))
			    ret)))))
     finally (return ret)))


(defun generate-kansuji-table (output-file &optional (package *package*))
  (with-open-file (stream output-file
			  :direction :output :if-exists :error
			  :if-does-not-exist :create)
    (let ((*print-circle* t))
      (format stream "~&~S~2%"
	      `(defparameter ,(intern (symbol-name '*kansuji-decimal-octets-vector*) package)
		 ,(make-kansuji-decimal-octets-vector)
		 "A vector of (<normal-octets> <financial-octets> <old-octets> <alternative-in-BMP-of-Unicode>)"
		 ))
      (format stream "~&~S~2%"
	      `(defparameter ,(intern (symbol-name '*kansuji-power-octets-alist*) package)
		 ,(make-kansuji-power-octets-alist)
		 "An alist of (<power> . (<normal-octets> <financial-octets> <old-octets> <alternative-in-BMP-of-Unicode>))")))))
