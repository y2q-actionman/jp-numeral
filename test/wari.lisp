;;; -*- coding: utf-8; -*-

(in-package :jp-numeral.test)

(defun test-wari-normal ()
  (flet ((jp-str/wn (num)
	   (with-output-to-string (stream)
	     (jp-numeral:wari stream num nil nil))))
    ;; integer
    (assert (equal "〇割" (jp-str/wn 0)))
    (assert (equal "十割" (jp-str/wn 1)))
    (assert (equal "二十割" (jp-str/wn 2)))
    (assert (equal "百割" (jp-str/wn 10)))
    (assert (equal "百十割" (jp-str/wn 11)))
    (assert (equal "百二十割" (jp-str/wn 12)))
    (assert (equal "百九十割" (jp-str/wn 19)))
    (assert (equal "二百割" (jp-str/wn 20)))
    (assert (equal "二百十割" (jp-str/wn 21)))
    (assert (equal "二百二十割" (jp-str/wn 22)))
    (assert (equal "千割" (jp-str/wn 100)))
    (assert (equal "二千十割" (jp-str/wn 201)))
    (assert (equal "一万割" (jp-str/wn 1000)))
    (assert (equal "一万千百十割" (jp-str/wn 1111)))
    (assert (equal "二万二千二百二十割" (jp-str/wn 2222)))
    (assert (equal "十万割" (jp-str/wn 10000)))
    (assert (equal "千二百十万割" (jp-str/wn 1210000)))
    (assert (equal "一億割" (jp-str/wn 10000000)))
    (assert (equal "一億二百三万四百割" (jp-str/wn 10203040)))

    (assert (equal "マイナス十割" (jp-str/wn -1)))
    (assert (equal "マイナス二十割" (jp-str/wn -2)))

    ;; ratio
    (assert (equal "五割" (jp-str/wn 1/2)))
    (assert (equal "三分の十割" (jp-str/wn 1/3)))
    (assert (equal "二分の五割" (jp-str/wn 1/4)))
    (assert (equal "二割" (jp-str/wn 1/5)))
    (assert (equal "三分の五割" (jp-str/wn 1/6)))
    (assert (equal "七分の十割" (jp-str/wn 1/7)))
    (assert (equal "四分の五割" (jp-str/wn 1/8)))
    (assert (equal "九分の十割" (jp-str/wn 1/9)))
    (assert (equal "一割" (jp-str/wn 1/10)))
    (assert (equal "十分の一割" (jp-str/wn 1/100)))
    (assert (equal "百分の一割" (jp-str/wn 1/1000)))
    (assert (equal "千分の一割" (jp-str/wn 1/10000)))
    (assert (equal "一万分の一割" (jp-str/wn 1/100000)))
    (assert (equal "十一割" (jp-str/wn 11/10)))
    (assert (equal "二十五割" (jp-str/wn 5/2)))
    (assert (equal "十一分の五十割" (jp-str/wn 5/11)))

    (assert (equal "マイナス五割" (jp-str/wn -1/2)))
    (assert (equal "マイナス三分の十割" (jp-str/wn -1/3)))
    (assert (equal "マイナス二十五割" (jp-str/wn -5/2)))
    (assert (equal "マイナス十一分の五十割" (jp-str/wn -5/11)))

    ;; float
    (assert (equal "一割" (jp-str/wn 0.1)))
    (assert (equal "一割一分" (jp-str/wn 0.11)))
    (assert (equal "一割二分三厘" (jp-str/wn 0.123)))
    (assert (equal "三割五厘" (jp-str/wn 0.305)))
    (assert (equal "十二割" (jp-str/wn 1.2)))
    (assert (equal "十割三分" (jp-str/wn 1.03)))
    (assert (equal "〇割五厘" (jp-str/wn 0.005)))
    
    (assert (equal "マイナス一割" (jp-str/wn -0.1)))
    (assert (equal "マイナス十割三分" (jp-str/wn -1.03)))
    (assert (equal "マイナス〇割五厘" (jp-str/wn -0.005)))
    ;; 
    t))

(defun test-wari-formal ()
  (flet ((jp-str/wf (num)
	   (with-output-to-string (stream)
	     (jp-numeral:wari stream num t nil))))
    ;; integer
    (assert (equal "〇割" (jp-str/wf 0)))
    (assert (equal "拾割" (jp-str/wf 1)))
    (assert (equal "弐拾割" (jp-str/wf 2)))
    (assert (equal "壱百割" (jp-str/wf 10)))
    (assert (equal "壱百拾割" (jp-str/wf 11)))
    (assert (equal "壱百弐拾割" (jp-str/wf 12)))
    (assert (equal "壱百九拾割" (jp-str/wf 19)))
    (assert (equal "弐百割" (jp-str/wf 20)))
    (assert (equal "弐百拾割" (jp-str/wf 21)))
    (assert (equal "弐百弐拾割" (jp-str/wf 22)))
    (assert (equal "壱千割" (jp-str/wf 100)))
    (assert (equal "弐千拾割" (jp-str/wf 201)))
    (assert (equal "壱万割" (jp-str/wf 1000)))
    (assert (equal "壱万壱千壱百拾割" (jp-str/wf 1111)))
    (assert (equal "弐万弐千弐百弐拾割" (jp-str/wf 2222)))
    (assert (equal "拾万割" (jp-str/wf 10000)))
    (assert (equal "壱千弐百拾万割" (jp-str/wf 1210000)))
    (assert (equal "壱億割" (jp-str/wf 10000000)))
    (assert (equal "壱億弐百参万四百割" (jp-str/wf 10203040)))

    (assert (equal "マイナス拾割" (jp-str/wf -1)))
    (assert (equal "マイナス弐拾割" (jp-str/wf -2)))

    ;; ratio
    (assert (equal "五割" (jp-str/wf 1/2)))
    (assert (equal "参分の拾割" (jp-str/wf 1/3)))
    (assert (equal "弐分の五割" (jp-str/wf 1/4)))
    (assert (equal "弐割" (jp-str/wf 1/5)))
    (assert (equal "参分の五割" (jp-str/wf 1/6)))
    (assert (equal "七分の拾割" (jp-str/wf 1/7)))
    (assert (equal "四分の五割" (jp-str/wf 1/8)))
    (assert (equal "九分の拾割" (jp-str/wf 1/9)))
    (assert (equal "壱割" (jp-str/wf 1/10)))
    (assert (equal "拾分の壱割" (jp-str/wf 1/100)))
    (assert (equal "壱百分の壱割" (jp-str/wf 1/1000)))
    (assert (equal "壱千分の壱割" (jp-str/wf 1/10000)))
    (assert (equal "壱万分の壱割" (jp-str/wf 1/100000)))
    (assert (equal "拾壱割" (jp-str/wf 11/10)))
    (assert (equal "弐拾五割" (jp-str/wf 5/2)))
    (assert (equal "拾壱分の五拾割" (jp-str/wf 5/11)))

    (assert (equal "マイナス五割" (jp-str/wf -1/2)))
    (assert (equal "マイナス参分の拾割" (jp-str/wf -1/3)))
    (assert (equal "マイナス弐拾五割" (jp-str/wf -5/2)))
    (assert (equal "マイナス拾壱分の五拾割" (jp-str/wf -5/11)))

    ;; float
    (assert (equal "壱割" (jp-str/wf 0.1)))
    (assert (equal "壱割壱分" (jp-str/wf 0.11)))
    (assert (equal "壱割弐分参厘" (jp-str/wf 0.123)))
    (assert (equal "参割五厘" (jp-str/wf 0.305)))
    (assert (equal "拾弐割" (jp-str/wf 1.2)))
    (assert (equal "拾割参分" (jp-str/wf 1.03)))
    (assert (equal "〇割五厘" (jp-str/wf 0.005)))
    
    (assert (equal "マイナス壱割" (jp-str/wf -0.1)))
    (assert (equal "マイナス拾割参分" (jp-str/wf -1.03)))
    (assert (equal "マイナス〇割五厘" (jp-str/wf -0.005)))
    ;; 
    t))

(defun test-wari-old ()
  (flet ((jp-str/wo (num)
	   (with-output-to-string (stream)
	     (jp-numeral:wari stream num nil t))))
    ;; integer
    (assert (equal "零割" (jp-str/wo 0)))
    (assert (equal "拾割" (jp-str/wo 1)))
    (assert (equal "貳拾割" (jp-str/wo 2)))
    (assert (equal "壹佰割" (jp-str/wo 10)))
    (assert (equal "壹佰拾割" (jp-str/wo 11)))
    (assert (equal "壹佰貳拾割" (jp-str/wo 12)))
    (assert (equal "壹佰玖拾割" (jp-str/wo 19)))
    (assert (equal "貳佰割" (jp-str/wo 20)))
    (assert (equal "貳佰拾割" (jp-str/wo 21)))
    (assert (equal "貳佰貳拾割" (jp-str/wo 22)))
    (assert (equal "壹仟割" (jp-str/wo 100)))
    (assert (equal "貳仟拾割" (jp-str/wo 201)))
    (assert (equal "壹萬割" (jp-str/wo 1000)))
    (assert (equal "壹萬壹仟壹佰拾割" (jp-str/wo 1111)))
    (assert (equal "貳萬貳仟貳佰貳拾割" (jp-str/wo 2222)))
    (assert (equal "拾萬割" (jp-str/wo 10000)))
    (assert (equal "壹仟貳佰拾萬割" (jp-str/wo 1210000)))
    (assert (equal "壹億割" (jp-str/wo 10000000)))
    (assert (equal "壹億貳佰參萬肆佰割" (jp-str/wo 10203040)))

    (assert (equal "負之拾割" (jp-str/wo -1)))
    (assert (equal "負之貳拾割" (jp-str/wo -2)))

    ;; ratio
    (assert (equal "伍割" (jp-str/wo 1/2)))
    (assert (equal "參分之拾割" (jp-str/wo 1/3)))
    (assert (equal "貳分之伍割" (jp-str/wo 1/4)))
    (assert (equal "貳割" (jp-str/wo 1/5)))
    (assert (equal "參分之伍割" (jp-str/wo 1/6)))
    (assert (equal "柒分之拾割" (jp-str/wo 1/7)))
    (assert (equal "肆分之伍割" (jp-str/wo 1/8)))
    (assert (equal "玖分之拾割" (jp-str/wo 1/9)))
    (assert (equal "壹割" (jp-str/wo 1/10)))
    (assert (equal "拾分之壹割" (jp-str/wo 1/100)))
    (assert (equal "壹佰分之壹割" (jp-str/wo 1/1000)))
    (assert (equal "壹仟分之壹割" (jp-str/wo 1/10000)))
    (assert (equal "壹萬分之壹割" (jp-str/wo 1/100000)))
    (assert (equal "拾壹割" (jp-str/wo 11/10)))
    (assert (equal "貳拾伍割" (jp-str/wo 5/2)))
    (assert (equal "拾壹分之伍拾割" (jp-str/wo 5/11)))

    (assert (equal "負之伍割" (jp-str/wo -1/2)))
    (assert (equal "負之參分之拾割" (jp-str/wo -1/3)))
    (assert (equal "負之貳拾伍割" (jp-str/wo -5/2)))
    (assert (equal "負之拾壹分之伍拾割" (jp-str/wo -5/11)))

    ;; float
    (assert (equal "壹割" (jp-str/wo 0.1)))
    (assert (equal "壹割壹分" (jp-str/wo 0.11)))
    (assert (equal "壹割貳分參釐" (jp-str/wo 0.123)))
    (assert (equal "參割伍釐" (jp-str/wo 0.305)))
    (assert (equal "拾貳割" (jp-str/wo 1.2)))
    (assert (equal "拾割參分" (jp-str/wo 1.03)))
    (assert (equal "零割伍釐" (jp-str/wo 0.005)))
    
    (assert (equal "負之壹割" (jp-str/wo -0.1)))
    (assert (equal "負之拾割參分" (jp-str/wo -1.03)))
    (assert (equal "負之零割伍釐" (jp-str/wo -0.005)))
    ;; 
    t))


;; TODO: positional


(defun test-wari-fallback ()
  (flet ((jp-str/wn (num)
	   (with-output-to-string (stream)
	     (jp-numeral:wari stream num nil nil))))
    ;; too big num
    (assert (equal "十無量大数割" (jp-str/wn (expt 10 68))))
    (assert (equal "一〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇割"
		   (jp-str/wn (expt 10 72))))
    (assert (equal "マイナス十無量大数割" (jp-str/wn (- (expt 10 68)))))
    (assert (equal "−一〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇割"
		   (jp-str/wn (- (expt 10 72)))))
    ;; ;; too small num
    ;; (assert (equal "一清浄割" (jp-str/wn 0.0000000000000000000001)))
    ;; (assert (equal (format nil "~A割" 0.00000000000000000000001)
    ;; 		   (jp-str/wn 0.00000000000000000000001)))
    ;; (assert (equal "マイナス一清浄割" (jp-str/wn -0.0000000000000000000001)))
    ;; (assert (equal (format nil "~A割" -0.00000000000000000000001)
    ;; 		   (jp-str/wn -0.00000000000000000000001)))
    ;; complex
    (assert (equal (format nil "~A割" #c(1 1))
		   (jp-str/wn #c(1 1))))
    ;;
    t))

(defun test-wari-parameters ()
  ;; [Integer]
  ;; digits-after-dot -- ignored
  (assert (equal "一割" (jp-str/w 1 :digits-after-dot nil)))
  (assert (equal "一割" (jp-str/w 1 :digits-after-dot 0)))
  (assert (equal "一割" (jp-str/w 1 :digits-after-dot 1)))
  (assert (equal "一割" (jp-str/w 1 :digits-after-dot -1)))
  ;; radix-point
  (assert (equal "一割" (jp-str/w 1 :radix-point nil)))
  (assert (equal "一a割" (jp-str/w 1 :radix-point #\a)))
  (assert (equal "一abc割" (jp-str/w 1 :radix-point "abc")))

  ;; [Rational]
  ;; digits-after-dot -- ignored
  (assert (equal "二分の一割" (jp-str/w 1/2 :digits-after-dot nil)))
  (assert (equal "二分の一割" (jp-str/w 1/2 :digits-after-dot 0)))
  (assert (equal "二分の一割" (jp-str/w 1/2 :digits-after-dot 1)))
  (assert (equal "二分の一割" (jp-str/w 1/2 :digits-after-dot -1)))
  ;; radix-point -- ignored
  (assert (equal "二分の一割" (jp-str/w 1/2 :radix-point nil)))
  (assert (equal "二分の一割" (jp-str/w 1/2 :radix-point #\a)))
  (assert (equal "二分の一割" (jp-str/w 1/2 :radix-point "abc")))

  ;; [Float]
  ;; digits-after-dot
  (assert (equal "十二・三分四厘割" (jp-str/w 12.34 :digits-after-dot nil)))
  (assert (equal "十二割" (jp-str/w 12.34 :digits-after-dot 0)))
  (assert (equal "十二・三分割" (jp-str/w 12.34 :digits-after-dot 1)))
  (assert (equal "十二・三分四厘割" (jp-str/w 12.34 :digits-after-dot 2)))
  (assert (equal "十二・三分四厘割" (jp-str/w 12.34 :digits-after-dot 3)))
  (assert (equal "十割" (jp-str/w 12.34 :digits-after-dot -1)))
  (assert (equal "〇割" (jp-str/w 12.34 :digits-after-dot -2)))
  ;; radix-point
  (assert (equal "十二・三分四厘割" (jp-str/w 12.34 :radix-point nil)))
  (assert (equal "十二a三分四厘割" (jp-str/w 12.34 :radix-point #\a)))
  (assert (equal "十二abc三分四厘割" (jp-str/w 12.34 :radix-point "abc")))
  ;; 
  t)

(defun test-wari ()
  (and (test-wari-normal)
       (test-wari-formal)
       (test-wari-old)
       ;; TODO: positional
       (test-wari-fallback)
       t))
