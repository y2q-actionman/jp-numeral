;;; -*- coding: utf-8; -*-

(in-package :jp-numeral.test)

(defun test-wari-normal ()
  ;; integer
  (assert (equal "〇割" (wari-str :normal 0)))
  (assert (equal "十割" (wari-str :normal 1)))
  (assert (equal "二十割" (wari-str :normal 2)))
  (assert (equal "百割" (wari-str :normal 10)))
  (assert (equal "百十割" (wari-str :normal 11)))
  (assert (equal "百二十割" (wari-str :normal 12)))
  (assert (equal "百九十割" (wari-str :normal 19)))
  (assert (equal "二百割" (wari-str :normal 20)))
  (assert (equal "二百十割" (wari-str :normal 21)))
  (assert (equal "二百二十割" (wari-str :normal 22)))
  (assert (equal "千割" (wari-str :normal 100)))
  (assert (equal "二千十割" (wari-str :normal 201)))
  (assert (equal "一万割" (wari-str :normal 1000)))
  (assert (equal "一万千百十割" (wari-str :normal 1111)))
  (assert (equal "二万二千二百二十割" (wari-str :normal 2222)))
  (assert (equal "十万割" (wari-str :normal 10000)))
  (assert (equal "千二百十万割" (wari-str :normal 1210000)))
  (assert (equal "一億割" (wari-str :normal 10000000)))
  (assert (equal "一億二百三万四百割" (wari-str :normal 10203040)))

  (assert (equal "マイナス十割" (wari-str :normal -1)))
  (assert (equal "マイナス二十割" (wari-str :normal -2)))

  ;; ratio
  (assert (equal "五割" (wari-str :normal 1/2)))
  (assert (equal "三分の十割" (wari-str :normal 1/3)))
  (assert (equal "二分の五割" (wari-str :normal 1/4)))
  (assert (equal "二割" (wari-str :normal 1/5)))
  (assert (equal "三分の五割" (wari-str :normal 1/6)))
  (assert (equal "七分の十割" (wari-str :normal 1/7)))
  (assert (equal "四分の五割" (wari-str :normal 1/8)))
  (assert (equal "九分の十割" (wari-str :normal 1/9)))
  (assert (equal "一割" (wari-str :normal 1/10)))
  (assert (equal "十分の一割" (wari-str :normal 1/100)))
  (assert (equal "百分の一割" (wari-str :normal 1/1000)))
  (assert (equal "千分の一割" (wari-str :normal 1/10000)))
  (assert (equal "一万分の一割" (wari-str :normal 1/100000)))
  (assert (equal "十一割" (wari-str :normal 11/10)))
  (assert (equal "二十五割" (wari-str :normal 5/2)))
  (assert (equal "十一分の五十割" (wari-str :normal 5/11)))

  (assert (equal "マイナス五割" (wari-str :normal -1/2)))
  (assert (equal "マイナス三分の十割" (wari-str :normal -1/3)))
  (assert (equal "マイナス二十五割" (wari-str :normal -5/2)))
  (assert (equal "マイナス十一分の五十割" (wari-str :normal -5/11)))

  ;; float
  (assert (equal "一割" (wari-str :normal 0.1)))
  (assert (equal "一割一分" (wari-str :normal 0.11)))
  (assert (equal "一割二分三厘" (wari-str :normal 0.123)))
  (assert (equal "三割五厘" (wari-str :normal 0.305)))
  (assert (equal "十二割" (wari-str :normal 1.2)))
  (assert (equal "十割三分" (wari-str :normal 1.03)))
  (assert (equal "〇割五厘" (wari-str :normal 0.005)))
  
  (assert (equal "マイナス一割" (wari-str :normal -0.1)))
  (assert (equal "マイナス十割三分" (wari-str :normal -1.03)))
  (assert (equal "マイナス〇割五厘" (wari-str :normal -0.005)))
  ;; 
  t)

(defun test-wari-formal ()
  ;; integer
  (assert (equal "〇割" (wari-str :formal 0)))
  (assert (equal "拾割" (wari-str :formal 1)))
  (assert (equal "弐拾割" (wari-str :formal 2)))
  (assert (equal "壱百割" (wari-str :formal 10)))
  (assert (equal "壱百拾割" (wari-str :formal 11)))
  (assert (equal "壱百弐拾割" (wari-str :formal 12)))
  (assert (equal "壱百九拾割" (wari-str :formal 19)))
  (assert (equal "弐百割" (wari-str :formal 20)))
  (assert (equal "弐百拾割" (wari-str :formal 21)))
  (assert (equal "弐百弐拾割" (wari-str :formal 22)))
  (assert (equal "壱千割" (wari-str :formal 100)))
  (assert (equal "弐千拾割" (wari-str :formal 201)))
  (assert (equal "壱万割" (wari-str :formal 1000)))
  (assert (equal "壱万壱千壱百拾割" (wari-str :formal 1111)))
  (assert (equal "弐万弐千弐百弐拾割" (wari-str :formal 2222)))
  (assert (equal "拾万割" (wari-str :formal 10000)))
  (assert (equal "壱千弐百拾万割" (wari-str :formal 1210000)))
  (assert (equal "壱億割" (wari-str :formal 10000000)))
  (assert (equal "壱億弐百参万四百割" (wari-str :formal 10203040)))

  (assert (equal "マイナス拾割" (wari-str :formal -1)))
  (assert (equal "マイナス弐拾割" (wari-str :formal -2)))

  ;; ratio
  (assert (equal "五割" (wari-str :formal 1/2)))
  (assert (equal "参分の拾割" (wari-str :formal 1/3)))
  (assert (equal "弐分の五割" (wari-str :formal 1/4)))
  (assert (equal "弐割" (wari-str :formal 1/5)))
  (assert (equal "参分の五割" (wari-str :formal 1/6)))
  (assert (equal "七分の拾割" (wari-str :formal 1/7)))
  (assert (equal "四分の五割" (wari-str :formal 1/8)))
  (assert (equal "九分の拾割" (wari-str :formal 1/9)))
  (assert (equal "壱割" (wari-str :formal 1/10)))
  (assert (equal "拾分の壱割" (wari-str :formal 1/100)))
  (assert (equal "壱百分の壱割" (wari-str :formal 1/1000)))
  (assert (equal "壱千分の壱割" (wari-str :formal 1/10000)))
  (assert (equal "壱万分の壱割" (wari-str :formal 1/100000)))
  (assert (equal "拾壱割" (wari-str :formal 11/10)))
  (assert (equal "弐拾五割" (wari-str :formal 5/2)))
  (assert (equal "拾壱分の五拾割" (wari-str :formal 5/11)))

  (assert (equal "マイナス五割" (wari-str :formal -1/2)))
  (assert (equal "マイナス参分の拾割" (wari-str :formal -1/3)))
  (assert (equal "マイナス弐拾五割" (wari-str :formal -5/2)))
  (assert (equal "マイナス拾壱分の五拾割" (wari-str :formal -5/11)))

  ;; float
  (assert (equal "壱割" (wari-str :formal 0.1)))
  (assert (equal "壱割壱分" (wari-str :formal 0.11)))
  (assert (equal "壱割弐分参厘" (wari-str :formal 0.123)))
  (assert (equal "参割五厘" (wari-str :formal 0.305)))
  (assert (equal "拾弐割" (wari-str :formal 1.2)))
  (assert (equal "拾割参分" (wari-str :formal 1.03)))
  (assert (equal "〇割五厘" (wari-str :formal 0.005)))
  
  (assert (equal "マイナス壱割" (wari-str :formal -0.1)))
  (assert (equal "マイナス拾割参分" (wari-str :formal -1.03)))
  (assert (equal "マイナス〇割五厘" (wari-str :formal -0.005)))
  ;; 
  t)

(defun test-wari-old ()
  ;; integer
  (assert (equal "零割" (wari-str :old 0)))
  (assert (equal "拾割" (wari-str :old 1)))
  (assert (equal "貳拾割" (wari-str :old 2)))
  (assert (equal "壹佰割" (wari-str :old 10)))
  (assert (equal "壹佰拾割" (wari-str :old 11)))
  (assert (equal "壹佰貳拾割" (wari-str :old 12)))
  (assert (equal "壹佰玖拾割" (wari-str :old 19)))
  (assert (equal "貳佰割" (wari-str :old 20)))
  (assert (equal "貳佰拾割" (wari-str :old 21)))
  (assert (equal "貳佰貳拾割" (wari-str :old 22)))
  (assert (equal "壹仟割" (wari-str :old 100)))
  (assert (equal "貳仟拾割" (wari-str :old 201)))
  (assert (equal "壹萬割" (wari-str :old 1000)))
  (assert (equal "壹萬壹仟壹佰拾割" (wari-str :old 1111)))
  (assert (equal "貳萬貳仟貳佰貳拾割" (wari-str :old 2222)))
  (assert (equal "拾萬割" (wari-str :old 10000)))
  (assert (equal "壹仟貳佰拾萬割" (wari-str :old 1210000)))
  (assert (equal "壹億割" (wari-str :old 10000000)))
  (assert (equal "壹億貳佰參萬肆佰割" (wari-str :old 10203040)))

  (assert (equal "負之拾割" (wari-str :old -1)))
  (assert (equal "負之貳拾割" (wari-str :old -2)))

  ;; ratio
  (assert (equal "伍割" (wari-str :old 1/2)))
  (assert (equal "參分之拾割" (wari-str :old 1/3)))
  (assert (equal "貳分之伍割" (wari-str :old 1/4)))
  (assert (equal "貳割" (wari-str :old 1/5)))
  (assert (equal "參分之伍割" (wari-str :old 1/6)))
  (assert (equal "柒分之拾割" (wari-str :old 1/7)))
  (assert (equal "肆分之伍割" (wari-str :old 1/8)))
  (assert (equal "玖分之拾割" (wari-str :old 1/9)))
  (assert (equal "壹割" (wari-str :old 1/10)))
  (assert (equal "拾分之壹割" (wari-str :old 1/100)))
  (assert (equal "壹佰分之壹割" (wari-str :old 1/1000)))
  (assert (equal "壹仟分之壹割" (wari-str :old 1/10000)))
  (assert (equal "壹萬分之壹割" (wari-str :old 1/100000)))
  (assert (equal "拾壹割" (wari-str :old 11/10)))
  (assert (equal "貳拾伍割" (wari-str :old 5/2)))
  (assert (equal "拾壹分之伍拾割" (wari-str :old 5/11)))

  (assert (equal "負之伍割" (wari-str :old -1/2)))
  (assert (equal "負之參分之拾割" (wari-str :old -1/3)))
  (assert (equal "負之貳拾伍割" (wari-str :old -5/2)))
  (assert (equal "負之拾壹分之伍拾割" (wari-str :old -5/11)))

  ;; float
  (assert (equal "壹割" (wari-str :old 0.1)))
  (assert (equal "壹割壹分" (wari-str :old 0.11)))
  (assert (equal "壹割貳分參釐" (wari-str :old 0.123)))
  (assert (equal "參割伍釐" (wari-str :old 0.305)))
  (assert (equal "拾貳割" (wari-str :old 1.2)))
  (assert (equal "拾割參分" (wari-str :old 1.03)))
  (assert (equal "零割伍釐" (wari-str :old 0.005)))
  
  (assert (equal "負之壹割" (wari-str :old -0.1)))
  (assert (equal "負之拾割參分" (wari-str :old -1.03)))
  (assert (equal "負之零割伍釐" (wari-str :old -0.005)))
  ;; 
  t)


;; TODO: positional


(defun test-wari-fallback ()
  ;; too big num
  (assert (equal "十無量大数割" (wari-str :normal (expt 10 68))))
  (assert (equal "一〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇割"
		 (wari-str :normal (expt 10 72))))
  (assert (equal "マイナス十無量大数割" (wari-str :normal (- (expt 10 68)))))
  (assert (equal "−一〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇割"
		 (wari-str :normal (- (expt 10 72)))))
  ;; ;; too small num
  (assert (equal "〇割一虚空" (wari-str :normal (float (expt 10 -21)))))
  (assert (equal "〇割一清浄" (wari-str :normal (float (expt 10 -22)))))
  (assert (equal "〇割〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇一"
		 (wari-str :normal (float (expt 10 -23)))))
  (assert (equal "マイナス〇割一虚空" (wari-str :normal (- (float (expt 10 -21))))))
  (assert (equal "マイナス〇割一清浄" (wari-str :normal (- (float (expt 10 -22))))))
  (assert (equal "−〇割〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇一"
		 (wari-str :normal (- (float (expt 10 -23))))))
  ;; complex
  (assert (equal (format nil "~A" #c(1 1))
		 (wari-str :normal #c(1 1))))
  ;;
  t)

(defun test-wari-parameters ()
 ;; [Integer]
 ;; digits-after-dot -- ignored
 (assert (equal "一割" (wari-str :normal 1/10 :digits-after-dot nil)))
 (assert (equal "一割" (wari-str :normal 1/10 :digits-after-dot nil)))
 (assert (equal "一割" (wari-str :normal 1/10 :digits-after-dot 0)))
 (assert (equal "一割" (wari-str :normal 1/10 :digits-after-dot 1)))
 (assert (equal "一割" (wari-str :normal 1/10 :digits-after-dot -1)))

 ;; [Rational]
 ;; digits-after-dot -- ignored
 (assert (equal "二分の一割" (wari-str :normal 1/20 :digits-after-dot nil)))
 (assert (equal "二分の一割" (wari-str :normal 1/20 :digits-after-dot 0)))
 (assert (equal "二分の一割" (wari-str :normal 1/20 :digits-after-dot 1)))
 (assert (equal "二分の一割" (wari-str :normal 1/20 :digits-after-dot -1)))

 ;; [Float]
 ;; digits-after-dot
 (assert (equal "十二割三分四厘" (wari-str :normal 1.234 :digits-after-dot nil)))
 (assert (equal "十二割" (wari-str :normal 1.234 :digits-after-dot 0)))
 (assert (equal "十二割三分" (wari-str :normal 1.234 :digits-after-dot 1)))
 (assert (equal "十二割三分四厘" (wari-str :normal 1.234 :digits-after-dot 2)))
 (assert (equal "十二割三分四厘" (wari-str :normal 1.234 :digits-after-dot 3)))
 (assert (equal "十割" (wari-str :normal 1.234 :digits-after-dot -1)))
 (assert (equal "〇割" (wari-str :normal 1.234 :digits-after-dot -2)))
 ;; 
 t)

(defun test-wari ()
  (and (test-wari-normal)
       (test-wari-formal)
       (test-wari-old)
       ;; TODO: positional
       (test-wari-fallback)
       (test-wari-parameters)
       t))
