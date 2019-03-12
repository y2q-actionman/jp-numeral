;;; -*- coding: utf-8; -*-

(in-package :jp-numeral.test)

(test test-wari-normal
  ;; integer
  (is (equal "〇割" (wari-str :normal 0)))
  (is (equal "十割" (wari-str :normal 1)))
  (is (equal "二十割" (wari-str :normal 2)))
  (is (equal "百割" (wari-str :normal 10)))
  (is (equal "百十割" (wari-str :normal 11)))
  (is (equal "百二十割" (wari-str :normal 12)))
  (is (equal "百九十割" (wari-str :normal 19)))
  (is (equal "二百割" (wari-str :normal 20)))
  (is (equal "二百十割" (wari-str :normal 21)))
  (is (equal "二百二十割" (wari-str :normal 22)))
  (is (equal "千割" (wari-str :normal 100)))
  (is (equal "二千十割" (wari-str :normal 201)))
  (is (equal "一万割" (wari-str :normal 1000)))
  (is (equal "一万千百十割" (wari-str :normal 1111)))
  (is (equal "二万二千二百二十割" (wari-str :normal 2222)))
  (is (equal "十万割" (wari-str :normal 10000)))
  (is (equal "千二百十万割" (wari-str :normal 1210000)))
  (is (equal "一億割" (wari-str :normal 10000000)))
  (is (equal "一億二百三万四百割" (wari-str :normal 10203040)))

  (is (equal "マイナス十割" (wari-str :normal -1)))
  (is (equal "マイナス二十割" (wari-str :normal -2)))

  ;; ratio
  (is (equal "五割" (wari-str :normal 1/2)))
  (is (equal "三分の十割" (wari-str :normal 1/3)))
  (is (equal "二分の五割" (wari-str :normal 1/4)))
  (is (equal "二割" (wari-str :normal 1/5)))
  (is (equal "三分の五割" (wari-str :normal 1/6)))
  (is (equal "七分の十割" (wari-str :normal 1/7)))
  (is (equal "四分の五割" (wari-str :normal 1/8)))
  (is (equal "九分の十割" (wari-str :normal 1/9)))
  (is (equal "一割" (wari-str :normal 1/10)))
  (is (equal "十分の一割" (wari-str :normal 1/100)))
  (is (equal "百分の一割" (wari-str :normal 1/1000)))
  (is (equal "千分の一割" (wari-str :normal 1/10000)))
  (is (equal "一万分の一割" (wari-str :normal 1/100000)))
  (is (equal "十一割" (wari-str :normal 11/10)))
  (is (equal "二十五割" (wari-str :normal 5/2)))
  (is (equal "十一分の五十割" (wari-str :normal 5/11)))

  (is (equal "マイナス五割" (wari-str :normal -1/2)))
  (is (equal "マイナス三分の十割" (wari-str :normal -1/3)))
  (is (equal "マイナス二十五割" (wari-str :normal -5/2)))
  (is (equal "マイナス十一分の五十割" (wari-str :normal -5/11)))

  ;; float
  (is (equal "一割" (wari-str :normal 0.1)))
  (is (equal "一割一分" (wari-str :normal 0.11)))
  (is (equal "一割二分三厘" (wari-str :normal 0.123)))
  (is (equal "三割五厘" (wari-str :normal 0.305)))
  (is (equal "十二割" (wari-str :normal 1.2)))
  (is (equal "十割三分" (wari-str :normal 1.03)))
  (is (equal "〇割五厘" (wari-str :normal 0.005)))
  
  (is (equal "マイナス一割" (wari-str :normal -0.1)))
  (is (equal "マイナス十割三分" (wari-str :normal -1.03)))
  (is (equal "マイナス〇割五厘" (wari-str :normal -0.005)))
  ;; 
  t)

(test test-wari-formal
  ;; integer
  (is (equal "〇割" (wari-str :formal 0)))
  (is (equal "拾割" (wari-str :formal 1)))
  (is (equal "弐拾割" (wari-str :formal 2)))
  (is (equal "壱百割" (wari-str :formal 10)))
  (is (equal "壱百拾割" (wari-str :formal 11)))
  (is (equal "壱百弐拾割" (wari-str :formal 12)))
  (is (equal "壱百九拾割" (wari-str :formal 19)))
  (is (equal "弐百割" (wari-str :formal 20)))
  (is (equal "弐百拾割" (wari-str :formal 21)))
  (is (equal "弐百弐拾割" (wari-str :formal 22)))
  (is (equal "壱千割" (wari-str :formal 100)))
  (is (equal "弐千拾割" (wari-str :formal 201)))
  (is (equal "壱万割" (wari-str :formal 1000)))
  (is (equal "壱万壱千壱百拾割" (wari-str :formal 1111)))
  (is (equal "弐万弐千弐百弐拾割" (wari-str :formal 2222)))
  (is (equal "拾万割" (wari-str :formal 10000)))
  (is (equal "壱千弐百拾万割" (wari-str :formal 1210000)))
  (is (equal "壱億割" (wari-str :formal 10000000)))
  (is (equal "壱億弐百参万四百割" (wari-str :formal 10203040)))

  (is (equal "マイナス拾割" (wari-str :formal -1)))
  (is (equal "マイナス弐拾割" (wari-str :formal -2)))

  ;; ratio
  (is (equal "五割" (wari-str :formal 1/2)))
  (is (equal "参分の拾割" (wari-str :formal 1/3)))
  (is (equal "弐分の五割" (wari-str :formal 1/4)))
  (is (equal "弐割" (wari-str :formal 1/5)))
  (is (equal "参分の五割" (wari-str :formal 1/6)))
  (is (equal "七分の拾割" (wari-str :formal 1/7)))
  (is (equal "四分の五割" (wari-str :formal 1/8)))
  (is (equal "九分の拾割" (wari-str :formal 1/9)))
  (is (equal "壱割" (wari-str :formal 1/10)))
  (is (equal "拾分の壱割" (wari-str :formal 1/100)))
  (is (equal "壱百分の壱割" (wari-str :formal 1/1000)))
  (is (equal "壱千分の壱割" (wari-str :formal 1/10000)))
  (is (equal "壱万分の壱割" (wari-str :formal 1/100000)))
  (is (equal "拾壱割" (wari-str :formal 11/10)))
  (is (equal "弐拾五割" (wari-str :formal 5/2)))
  (is (equal "拾壱分の五拾割" (wari-str :formal 5/11)))

  (is (equal "マイナス五割" (wari-str :formal -1/2)))
  (is (equal "マイナス参分の拾割" (wari-str :formal -1/3)))
  (is (equal "マイナス弐拾五割" (wari-str :formal -5/2)))
  (is (equal "マイナス拾壱分の五拾割" (wari-str :formal -5/11)))

  ;; float
  (is (equal "壱割" (wari-str :formal 0.1)))
  (is (equal "壱割壱分" (wari-str :formal 0.11)))
  (is (equal "壱割弐分参厘" (wari-str :formal 0.123)))
  (is (equal "参割五厘" (wari-str :formal 0.305)))
  (is (equal "拾弐割" (wari-str :formal 1.2)))
  (is (equal "拾割参分" (wari-str :formal 1.03)))
  (is (equal "〇割五厘" (wari-str :formal 0.005)))
  
  (is (equal "マイナス壱割" (wari-str :formal -0.1)))
  (is (equal "マイナス拾割参分" (wari-str :formal -1.03)))
  (is (equal "マイナス〇割五厘" (wari-str :formal -0.005)))
  ;; 
  t)

(test test-wari-old
  ;; integer
  (is (equal "零割" (wari-str :old 0)))
  (is (equal "拾割" (wari-str :old 1)))
  (is (equal "貳拾割" (wari-str :old 2)))
  (is (equal "壹佰割" (wari-str :old 10)))
  (is (equal "壹佰拾割" (wari-str :old 11)))
  (is (equal "壹佰貳拾割" (wari-str :old 12)))
  (is (equal "壹佰玖拾割" (wari-str :old 19)))
  (is (equal "貳佰割" (wari-str :old 20)))
  (is (equal "貳佰拾割" (wari-str :old 21)))
  (is (equal "貳佰貳拾割" (wari-str :old 22)))
  (is (equal "壹仟割" (wari-str :old 100)))
  (is (equal "貳仟拾割" (wari-str :old 201)))
  (is (equal "壹萬割" (wari-str :old 1000)))
  (is (equal "壹萬壹仟壹佰拾割" (wari-str :old 1111)))
  (is (equal "貳萬貳仟貳佰貳拾割" (wari-str :old 2222)))
  (is (equal "拾萬割" (wari-str :old 10000)))
  (is (equal "壹仟貳佰拾萬割" (wari-str :old 1210000)))
  (is (equal "壹億割" (wari-str :old 10000000)))
  (is (equal "壹億貳佰參萬肆佰割" (wari-str :old 10203040)))

  (is (equal "負之拾割" (wari-str :old -1)))
  (is (equal "負之貳拾割" (wari-str :old -2)))

  ;; ratio
  (is (equal "伍割" (wari-str :old 1/2)))
  (is (equal "參分之拾割" (wari-str :old 1/3)))
  (is (equal "貳分之伍割" (wari-str :old 1/4)))
  (is (equal "貳割" (wari-str :old 1/5)))
  (is (equal "參分之伍割" (wari-str :old 1/6)))
  (is (equal "柒分之拾割" (wari-str :old 1/7)))
  (is (equal "肆分之伍割" (wari-str :old 1/8)))
  (is (equal "玖分之拾割" (wari-str :old 1/9)))
  (is (equal "壹割" (wari-str :old 1/10)))
  (is (equal "拾分之壹割" (wari-str :old 1/100)))
  (is (equal "壹佰分之壹割" (wari-str :old 1/1000)))
  (is (equal "壹仟分之壹割" (wari-str :old 1/10000)))
  (is (equal "壹萬分之壹割" (wari-str :old 1/100000)))
  (is (equal "拾壹割" (wari-str :old 11/10)))
  (is (equal "貳拾伍割" (wari-str :old 5/2)))
  (is (equal "拾壹分之伍拾割" (wari-str :old 5/11)))

  (is (equal "負之伍割" (wari-str :old -1/2)))
  (is (equal "負之參分之拾割" (wari-str :old -1/3)))
  (is (equal "負之貳拾伍割" (wari-str :old -5/2)))
  (is (equal "負之拾壹分之伍拾割" (wari-str :old -5/11)))

  ;; float
  (is (equal "壹割" (wari-str :old 0.1)))
  (is (equal "壹割壹分" (wari-str :old 0.11)))
  (is (equal "壹割貳分參釐" (wari-str :old 0.123)))
  (is (equal "參割伍釐" (wari-str :old 0.305)))
  (is (equal "拾貳割" (wari-str :old 1.2)))
  (is (equal "拾割參分" (wari-str :old 1.03)))
  (is (equal "零割伍釐" (wari-str :old 0.005)))
  
  (is (equal "負之壹割" (wari-str :old -0.1)))
  (is (equal "負之拾割參分" (wari-str :old -1.03)))
  (is (equal "負之零割伍釐" (wari-str :old -0.005)))
  ;; 
  t)

(test test-wari-positional
  ;; integer
  (is (equal "〇割" (wari-str :positional 0)))
  (is (equal "一〇割" (wari-str :positional 1)))
  (is (equal "二〇割" (wari-str :positional 2)))
  (is (equal "一〇〇割" (wari-str :positional 10)))
  (is (equal "一一〇割" (wari-str :positional 11)))
  (is (equal "一二〇割" (wari-str :positional 12)))
  (is (equal "一九〇割" (wari-str :positional 19)))
  (is (equal "二〇〇割" (wari-str :positional 20)))
  (is (equal "二一〇割" (wari-str :positional 21)))
  (is (equal "二二〇割" (wari-str :positional 22)))
  (is (equal "一〇〇〇割" (wari-str :positional 100)))
  (is (equal "二〇一〇割" (wari-str :positional 201)))
  (is (equal "一〇〇〇〇割" (wari-str :positional 1000)))
  (is (equal "一一一一〇割" (wari-str :positional 1111)))
  (is (equal "二二二二〇割" (wari-str :positional 2222)))
  (is (equal "一〇〇〇〇〇割" (wari-str :positional 10000)))
  (is (equal "一二一〇〇〇〇〇割" (wari-str :positional 1210000)))
  (is (equal "一〇〇〇〇〇〇〇〇割" (wari-str :positional 10000000)))
  (is (equal "一〇二〇三〇四〇〇割" (wari-str :positional 10203040)))

  (is (equal "−一〇割" (wari-str :positional -1)))
  (is (equal "−二〇割" (wari-str :positional -2)))

  ;; ratio
  (is (equal "五割" (wari-str :positional 1/2)))
  (is (equal "一〇／三割" (wari-str :positional 1/3)))
  (is (equal "五／二割" (wari-str :positional 1/4)))
  (is (equal "二割" (wari-str :positional 1/5)))
  (is (equal "五／三割" (wari-str :positional 1/6)))
  (is (equal "一〇／七割" (wari-str :positional 1/7)))
  (is (equal "五／四割" (wari-str :positional 1/8)))
  (is (equal "一〇／九割" (wari-str :positional 1/9)))
  (is (equal "一割" (wari-str :positional 1/10)))
  (is (equal "一／一〇割" (wari-str :positional 1/100)))
  (is (equal "一／一〇〇割" (wari-str :positional 1/1000)))
  (is (equal "一／一〇〇〇割" (wari-str :positional 1/10000)))
  (is (equal "一／一〇〇〇〇割" (wari-str :positional 1/100000)))
  (is (equal "一一割" (wari-str :positional 11/10)))
  (is (equal "二五割" (wari-str :positional 5/2)))
  (is (equal "五〇／一一割" (wari-str :positional 5/11)))

  (is (equal "−五割" (wari-str :positional -1/2)))
  (is (equal "−一〇／三割" (wari-str :positional -1/3)))
  (is (equal "−二五割" (wari-str :positional -5/2)))
  (is (equal "−五〇／一一割" (wari-str :positional -5/11)))

  ;; float
  (is (equal "一割" (wari-str :positional 0.1)))
  (is (equal "一割一" (wari-str :positional 0.11)))
  (is (equal "一割二三" (wari-str :positional 0.123)))
  (is (equal "三割〇五" (wari-str :positional 0.305)))
  (is (equal "一二割" (wari-str :positional 1.2)))
  (is (equal "一〇割三" (wari-str :positional 1.03)))
  (is (equal "〇割〇五" (wari-str :positional 0.005)))
  
  (is (equal "−一割" (wari-str :positional -0.1)))
  (is (equal "−一〇割三" (wari-str :positional -1.03)))
  (is (equal "−〇割〇五" (wari-str :positional -0.005)))
  ;; 
  t)

(test test-wari-fallback
  ;; too big num
  (is (equal "十無量大数割" (wari-str :normal (expt 10 68))))
  (is (equal "一〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇割"
	     (wari-str :normal (expt 10 72))))
  (is (equal "マイナス十無量大数割" (wari-str :normal (- (expt 10 68)))))
  (is (equal "−一〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇割"
	     (wari-str :normal (- (expt 10 72)))))
  ;; ;; too small num
  (is (equal "〇割一虚空" (wari-str :normal (float (expt 10 -21)))))
  (is (equal "〇割一清浄" (wari-str :normal (float (expt 10 -22)))))
  (is (equal "〇割〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇一"
	     (wari-str :normal (float (expt 10 -23)))))
  (is (equal "マイナス〇割一虚空" (wari-str :normal (- (float (expt 10 -21))))))
  (is (equal "マイナス〇割一清浄" (wari-str :normal (- (float (expt 10 -22))))))
  (is (equal "−〇割〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇一"
	     (wari-str :normal (- (float (expt 10 -23))))))
  ;; complex
  (signals error (wari-str :normal #c(1 1)))
  ;;
  t)

(test test-wari-parameters
  ;; [Integer]
  ;; digits-after-dot -- ignored
  (is (equal "一割" (wari-str :normal 1/10 :digits-after-dot nil)))
  (is (equal "一割" (wari-str :normal 1/10 :digits-after-dot nil)))
  (is (equal "一割" (wari-str :normal 1/10 :digits-after-dot 0)))
  (is (equal "一割" (wari-str :normal 1/10 :digits-after-dot 1)))
  #+allegro(is (equal "一割" (wari-str :normal 1/10 :digits-after-dot -1)))

  ;; [Rational]
  ;; digits-after-dot -- ignored
  (is (equal "二分の一割" (wari-str :normal 1/20 :digits-after-dot nil)))
  (is (equal "二分の一割" (wari-str :normal 1/20 :digits-after-dot 0)))
  (is (equal "二分の一割" (wari-str :normal 1/20 :digits-after-dot 1)))
  #+allegro(is (equal "二分の一割" (wari-str :normal 1/20 :digits-after-dot -1)))

  ;; [Float]
  ;; digits-after-dot
  (is (equal "十二割三分四厘" (wari-str :normal 1.234 :digits-after-dot nil)))
  (is (equal "十二割" (wari-str :normal 1.234 :digits-after-dot 0)))
  (is (equal "十二割三分" (wari-str :normal 1.234 :digits-after-dot 1)))
  (is (equal "十二割三分四厘" (wari-str :normal 1.234 :digits-after-dot 2)))
  (is (equal "十二割三分四厘" (wari-str :normal 1.234 :digits-after-dot 3)))
  #+allegro(is (equal "十割" (wari-str :normal 1.234 :digits-after-dot -1)))
  #+allegro(is (equal "〇割" (wari-str :normal 1.234 :digits-after-dot -2)))
  ;; 
  t)
