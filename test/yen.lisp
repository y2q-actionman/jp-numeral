;;; -*- coding: utf-8; -*-

(in-package :jp-numeral.test)

(defun test-yen-normal ()
  ;; integer
  (assert-equal "〇円" (yen-str :normal 0))
  (assert-equal "一円" (yen-str :normal 1))
  (assert-equal "二円" (yen-str :normal 2))
  (assert-equal "十円" (yen-str :normal 10))
  (assert-equal "十一円" (yen-str :normal 11))
  (assert-equal "十二円" (yen-str :normal 12))
  (assert-equal "十九円" (yen-str :normal 19))
  (assert-equal "二十円" (yen-str :normal 20))
  (assert-equal "二十一円" (yen-str :normal 21))
  (assert-equal "二十二円" (yen-str :normal 22))
  (assert-equal "百円" (yen-str :normal 100))
  (assert-equal "二百一円" (yen-str :normal 201))
  (assert-equal "千円" (yen-str :normal 1000))
  (assert-equal "千百十一円" (yen-str :normal 1111))
  (assert-equal "二千二百二十二円" (yen-str :normal 2222))
  (assert-equal "一万円" (yen-str :normal 10000))
  (assert-equal "百二十一万円" (yen-str :normal 1210000))
  (assert-equal "一千万円" (yen-str :normal 10000000))
  (assert-equal "千二十万三千四十円" (yen-str :normal 10203040))

  (assert-equal "マイナス一円" (yen-str :normal -1))
  (assert-equal "マイナス二円" (yen-str :normal -2))

  ;; ratio - rounded
  (assert-equal "五十銭" (yen-str :normal 1/2))
  (assert-equal "三十三銭" (yen-str :normal 1/3))
  (assert-equal "二十五銭" (yen-str :normal 1/4))
  (assert-equal "二十銭" (yen-str :normal 1/5))
  (assert-equal "十七銭" (yen-str :normal 1/6))
  (assert-equal "十四銭" (yen-str :normal 1/7))
  (assert-equal "十二銭" (yen-str :normal 1/8))
  (assert-equal "十一銭" (yen-str :normal 1/9))
  (assert-equal "十銭" (yen-str :normal 1/10))
  (assert-equal "一銭" (yen-str :normal 1/100))
  (assert-equal "〇円" (yen-str :normal 1/1000))
  (assert-equal "〇円" (yen-str :normal 1/10000))
  (assert-equal "〇円" (yen-str :normal 1/100000))
  (assert-equal "一円十銭" (yen-str :normal 11/10))
  (assert-equal "二円五十銭" (yen-str :normal 5/2))
  (assert-equal "四十五銭" (yen-str :normal 5/11))

  (assert-equal "マイナス五十銭" (yen-str :normal -1/2))
  (assert-equal "マイナス三十三銭" (yen-str :normal -1/3))
  (assert-equal "マイナス二円五十銭" (yen-str :normal -5/2))
  (assert-equal "マイナス四十五銭" (yen-str :normal -5/11))

  ;; float - TODO: digits 3
  (assert-equal "十銭" (yen-str :normal 0.1))
  (assert-equal "十一銭" (yen-str :normal 0.11))
  (assert-equal "十二銭" (yen-str :normal 0.123))
  (assert-equal "三十一銭" (yen-str :normal 0.305))
  (assert-equal "一円二十銭" (yen-str :normal 1.2))
  (assert-equal "一円三銭" (yen-str :normal 1.03))
  ;; (assert-equal "〇円一銭" (yen-str :normal 0.005)) ; TODO
  
  (assert-equal "マイナス十銭" (yen-str :normal -0.1))
  (assert-equal "マイナス一円三銭" (yen-str :normal -1.03))
  ;; (assert-equal "マイナス一銭" (yen-str :normal -0.005)) ; TODO
  ;; 
  t)

(defun test-yen-formal ()
  ;; (stub)
  ;; TODO
  ;; 
  t)

(defun test-yen-old ()
  ;; (stub)
  ;; TODO
  ;; 
  t)

(defun test-yen-positional ()
  ;; (stub)
  ;; TODO
  ;; 
  t)

(defun test-yen-fallback ()
  ;; too big num
  (assert-equal "一無量大数円" (yen-str :normal (expt 10 68)))
  (assert-equal "一〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇円"
		(yen-str :normal (expt 10 72)))
  (assert-equal "マイナス一無量大数円" (yen-str :normal (- (expt 10 68))))
  ;; (assert (equal "−一〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇円"
  ;; 		 (yen-str :normal (- (expt 10 72))))) ; TODO
  ;; ;; too small num
  (assert-equal "〇円" (yen-str :normal (float (expt 10 -21))))
  (assert-equal "〇円" (yen-str :normal (float (expt 10 -22))))
  (assert-equal "〇円" (yen-str :normal (float (expt 10 -23))))
  (assert-equal "〇円" (yen-str :normal (- (float (expt 10 -21)))))
  (assert-equal "〇円" (yen-str :normal (- (float (expt 10 -22)))))
  (assert-equal "〇円" (yen-str :normal (- (float (expt 10 -23)))))
  ;; ;; complex TODO
  ;; (assert-equal (format nil "~A" #c(1 1))
  ;; 		 (yen-str :normal #c(1 1))))
  ;;
  t)

(defun test-yen-parameters ()
  ;; (stub)
  ;; TODO
  ;; 
  t)

(defun test-yen ()
  (and (test-yen-normal)
       (test-yen-formal)
       (test-yen-old)
       (test-yen-positional)
       (test-yen-fallback)
       (test-yen-parameters)
       t))
