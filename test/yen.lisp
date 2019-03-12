;;; -*- coding: utf-8; -*-

(in-package :jp-numeral.test)

(test test-yen-normal
  ;; integer
  (is (equal "〇円" (yen-str :normal 0)))
  (is (equal "一円" (yen-str :normal 1)))
  (is (equal "二円" (yen-str :normal 2)))
  (is (equal "十円" (yen-str :normal 10)))
  (is (equal "十一円" (yen-str :normal 11)))
  (is (equal "十二円" (yen-str :normal 12)))
  (is (equal "十九円" (yen-str :normal 19)))
  (is (equal "二十円" (yen-str :normal 20)))
  (is (equal "二十一円" (yen-str :normal 21)))
  (is (equal "二十二円" (yen-str :normal 22)))
  (is (equal "百円" (yen-str :normal 100)))
  (is (equal "二百一円" (yen-str :normal 201)))
  (is (equal "千円" (yen-str :normal 1000)))
  (is (equal "千百十一円" (yen-str :normal 1111)))
  (is (equal "二千二百二十二円" (yen-str :normal 2222)))
  (is (equal "一万円" (yen-str :normal 10000)))
  (is (equal "百二十一万円" (yen-str :normal 1210000)))
  (is (equal "一千万円" (yen-str :normal 10000000)))
  (is (equal "千二十万三千四十円" (yen-str :normal 10203040)))

  (is (equal "マイナス一円" (yen-str :normal -1)))
  (is (equal "マイナス二円" (yen-str :normal -2)))

  ;; ratio 2
  (is (equal "五十銭" (yen-str :normal 1/2)))
  (is (equal "三十三銭" (yen-str :normal 1/3)))
  (is (equal "二十五銭" (yen-str :normal 1/4)))
  (is (equal "二十銭" (yen-str :normal 1/5)))
  (is (equal "十七銭" (yen-str :normal 1/6)))
  (is (equal "十四銭" (yen-str :normal 1/7)))
  (is (equal "十二銭" (yen-str :normal 1/8)))
  (is (equal "十一銭" (yen-str :normal 1/9)))
  (is (equal "十銭" (yen-str :normal 1/10)))
  (is (equal "一銭" (yen-str :normal 1/100)))
  (is (equal "〇円" (yen-str :normal 1/1000)))
  (is (equal "〇円" (yen-str :normal 1/10000)))
  (is (equal "〇円" (yen-str :normal 1/100000)))
  (is (equal "一円十銭" (yen-str :normal 11/10)))
  (is (equal "二円五十銭" (yen-str :normal 5/2)))
  (is (equal "四十五銭" (yen-str :normal 5/11)))
  (is (equal "一円" (yen-str :normal 1001/1000)))

  (is (equal "マイナス五十銭" (yen-str :normal -1/2)))
  (is (equal "マイナス三十三銭" (yen-str :normal -1/3)))
  (is (equal "マイナス二円五十銭" (yen-str :normal -5/2)))
  (is (equal "マイナス四十五銭" (yen-str :normal -5/11)))

  ;; ratio 3
  (is (equal "五十銭" (yen-str :normal 1/2 :digits-after-dot 3)))
  (is (equal "三十三銭三厘" (yen-str :normal 1/3 :digits-after-dot 3)))
  (is (equal "二十五銭" (yen-str :normal 1/4 :digits-after-dot 3)))
  (is (equal "二十銭" (yen-str :normal 1/5 :digits-after-dot 3)))
  (is (equal "十六銭七厘" (yen-str :normal 1/6 :digits-after-dot 3)))
  (is (equal "十四銭三厘" (yen-str :normal 1/7 :digits-after-dot 3)))
  (is (equal "十二銭五厘" (yen-str :normal 1/8 :digits-after-dot 3)))
  (is (equal "十一銭一厘" (yen-str :normal 1/9 :digits-after-dot 3)))
  (is (equal "十銭" (yen-str :normal 1/10 :digits-after-dot 3)))
  (is (equal "一銭" (yen-str :normal 1/100 :digits-after-dot 3)))
  (is (equal "一厘" (yen-str :normal 1/1000 :digits-after-dot 3)))
  (is (equal "〇円" (yen-str :normal 1/10000 :digits-after-dot 3)))
  (is (equal "〇円" (yen-str :normal 1/100000 :digits-after-dot 3)))
  (is (equal "一円十銭" (yen-str :normal 11/10 :digits-after-dot 3)))
  (is (equal "二円五十銭" (yen-str :normal 5/2 :digits-after-dot 3)))
  (is (equal "四十五銭五厘" (yen-str :normal 5/11 :digits-after-dot 3)))
  (is (equal "一円一厘" (yen-str :normal 1001/1000 :digits-after-dot 3)))

  (is (equal "マイナス五十銭" (yen-str :normal -1/2 :digits-after-dot 3)))
  (is (equal "マイナス三十三銭三厘" (yen-str :normal -1/3 :digits-after-dot 3)))
  (is (equal "マイナス二円五十銭" (yen-str :normal -5/2 :digits-after-dot 3)))
  (is (equal "マイナス四十五銭五厘" (yen-str :normal -5/11 :digits-after-dot 3)))

  ;; ratio 0
  ;; (is (equal "一円" (yen-str :normal 1/2 :digits-after-dot 0))) ; 1/2 is ambiguous
  (is (equal "〇円" (yen-str :normal 1/3 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :normal 1/4 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :normal 1/5 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :normal 1/6 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :normal 1/7 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :normal 1/8 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :normal 1/9 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :normal 1/10 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :normal 1/100 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :normal 1/1000 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :normal 1/10000 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :normal 1/100000 :digits-after-dot 0)))
  (is (equal "一円" (yen-str :normal 11/10 :digits-after-dot 0)))
  ;; (is (equal "三円" (yen-str :normal 5/2 :digits-after-dot 0))) ; 1/2 is ambiguous
  (is (equal "〇円" (yen-str :normal 5/11 :digits-after-dot 0)))
  (is (equal "一円" (yen-str :normal 1001/1000 :digits-after-dot 0)))

  ;; (is (equal "マイナス一円" (yen-str :normal -1/2 :digits-after-dot 0))) ; 1/2 is ambiguous
  (is (equal "〇円" (yen-str :normal -1/3 :digits-after-dot 0)))
  ;; (is (equal "マイナス三円" (yen-str :normal -5/2 :digits-after-dot 0))) ; 1/2 is ambiguous
  (is (equal "〇円" (yen-str :normal -5/11 :digits-after-dot 0)))

  ;; float 2
  (is (equal "十銭" (yen-str :normal 0.1)))
  (is (equal "十一銭" (yen-str :normal 0.11)))
  (is (equal "十二銭" (yen-str :normal 0.123)))
  (is (equal "三十一銭" (yen-str :normal 0.306)))
  (is (equal "一円二十銭" (yen-str :normal 1.2)))
  (is (equal "一円三銭" (yen-str :normal 1.03)))
  (is (equal "一銭" (yen-str :normal 0.006)))
  (is (equal "一円一銭" (yen-str :normal 1.006)))
  
  (is (equal "マイナス十銭" (yen-str :normal -0.1)))
  (is (equal "マイナス一円三銭" (yen-str :normal -1.03)))
  (is (equal "マイナス一銭" (yen-str :normal -0.006)))
  (is (equal "マイナス一円一銭" (yen-str :normal -1.006)))

  ;; float 3
  (is (equal "十銭" (yen-str :normal 0.1 :digits-after-dot 3)))
  (is (equal "十一銭" (yen-str :normal 0.11 :digits-after-dot 3)))
  (is (equal "十二銭三厘" (yen-str :normal 0.123 :digits-after-dot 3)))
  (is (equal "三十銭六厘" (yen-str :normal 0.306 :digits-after-dot 3)))
  (is (equal "一円二十銭" (yen-str :normal 1.2 :digits-after-dot 3)))
  (is (equal "一円三銭" (yen-str :normal 1.03 :digits-after-dot 3)))
  (is (equal "六厘" (yen-str :normal 0.006 :digits-after-dot 3)))
  (is (equal "一円六厘" (yen-str :normal 1.006 :digits-after-dot 3)))
  
  (is (equal "マイナス十銭" (yen-str :normal -0.1 :digits-after-dot 3)))
  (is (equal "マイナス一円三銭" (yen-str :normal -1.03 :digits-after-dot 3)))
  (is (equal "マイナス六厘" (yen-str :normal -0.006 :digits-after-dot 3)))
  (is (equal "マイナス一円六厘" (yen-str :normal -1.006 :digits-after-dot 3)))

  ;; float 0
  (is (equal "〇円" (yen-str :normal 0.1 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :normal 0.11 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :normal 0.123 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :normal 0.306 :digits-after-dot 0)))
  (is (equal "一円" (yen-str :normal 1.2 :digits-after-dot 0)))
  (is (equal "一円" (yen-str :normal 1.03 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :normal 0.006 :digits-after-dot 0)))
  (is (equal "一円" (yen-str :normal 1.006 :digits-after-dot 0)))
  
  (is (equal "〇円" (yen-str :normal -0.1 :digits-after-dot 0)))
  (is (equal "マイナス一円" (yen-str :normal -1.03 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :normal -0.006 :digits-after-dot 0)))
  (is (equal "マイナス一円" (yen-str :normal -1.006 :digits-after-dot 0)))
  ;; 
  t)

(test test-yen-formal
  ;; integer
  (is (equal "〇円" (yen-str :formal 0)))
  (is (equal "壱円" (yen-str :formal 1)))
  (is (equal "弐円" (yen-str :formal 2)))
  (is (equal "拾円" (yen-str :formal 10)))
  (is (equal "拾壱円" (yen-str :formal 11)))
  (is (equal "拾弐円" (yen-str :formal 12)))
  (is (equal "拾九円" (yen-str :formal 19)))
  (is (equal "弐拾円" (yen-str :formal 20)))
  (is (equal "弐拾壱円" (yen-str :formal 21)))
  (is (equal "弐拾弐円" (yen-str :formal 22)))
  (is (equal "壱百円" (yen-str :formal 100)))
  (is (equal "弐百壱円" (yen-str :formal 201)))
  (is (equal "壱千円" (yen-str :formal 1000)))
  (is (equal "壱千壱百拾壱円" (yen-str :formal 1111)))
  (is (equal "弐千弐百弐拾弐円" (yen-str :formal 2222)))
  (is (equal "壱万円" (yen-str :formal 10000)))
  (is (equal "壱百弐拾壱万円" (yen-str :formal 1210000)))
  (is (equal "壱千万円" (yen-str :formal 10000000)))
  (is (equal "壱千弐拾万参千四拾円" (yen-str :formal 10203040)))

  (is (equal "マイナス壱円" (yen-str :formal -1)))
  (is (equal "マイナス弐円" (yen-str :formal -2)))

  ;; ratio 2
  (is (equal "五拾銭" (yen-str :formal 1/2)))
  (is (equal "参拾参銭" (yen-str :formal 1/3)))
  (is (equal "弐拾五銭" (yen-str :formal 1/4)))
  (is (equal "弐拾銭" (yen-str :formal 1/5)))
  (is (equal "拾七銭" (yen-str :formal 1/6)))
  (is (equal "拾四銭" (yen-str :formal 1/7)))
  (is (equal "拾弐銭" (yen-str :formal 1/8)))
  (is (equal "拾壱銭" (yen-str :formal 1/9)))
  (is (equal "拾銭" (yen-str :formal 1/10)))
  (is (equal "壱銭" (yen-str :formal 1/100)))
  (is (equal "〇円" (yen-str :formal 1/1000)))
  (is (equal "〇円" (yen-str :formal 1/10000)))
  (is (equal "〇円" (yen-str :formal 1/100000)))
  (is (equal "壱円拾銭" (yen-str :formal 11/10)))
  (is (equal "弐円五拾銭" (yen-str :formal 5/2)))
  (is (equal "四拾五銭" (yen-str :formal 5/11)))
  (is (equal "壱円" (yen-str :formal 1001/1000)))

  (is (equal "マイナス五拾銭" (yen-str :formal -1/2)))
  (is (equal "マイナス参拾参銭" (yen-str :formal -1/3)))
  (is (equal "マイナス弐円五拾銭" (yen-str :formal -5/2)))
  (is (equal "マイナス四拾五銭" (yen-str :formal -5/11)))

  ;; ratio 3
  (is (equal "五拾銭" (yen-str :formal 1/2 :digits-after-dot 3)))
  (is (equal "参拾参銭参厘" (yen-str :formal 1/3 :digits-after-dot 3)))
  (is (equal "弐拾五銭" (yen-str :formal 1/4 :digits-after-dot 3)))
  (is (equal "弐拾銭" (yen-str :formal 1/5 :digits-after-dot 3)))
  (is (equal "拾六銭七厘" (yen-str :formal 1/6 :digits-after-dot 3)))
  (is (equal "拾四銭参厘" (yen-str :formal 1/7 :digits-after-dot 3)))
  (is (equal "拾弐銭五厘" (yen-str :formal 1/8 :digits-after-dot 3)))
  (is (equal "拾壱銭壱厘" (yen-str :formal 1/9 :digits-after-dot 3)))
  (is (equal "拾銭" (yen-str :formal 1/10 :digits-after-dot 3)))
  (is (equal "壱銭" (yen-str :formal 1/100 :digits-after-dot 3)))
  (is (equal "壱厘" (yen-str :formal 1/1000 :digits-after-dot 3)))
  (is (equal "〇円" (yen-str :formal 1/10000 :digits-after-dot 3)))
  (is (equal "〇円" (yen-str :formal 1/100000 :digits-after-dot 3)))
  (is (equal "壱円拾銭" (yen-str :formal 11/10 :digits-after-dot 3)))
  (is (equal "弐円五拾銭" (yen-str :formal 5/2 :digits-after-dot 3)))
  (is (equal "四拾五銭五厘" (yen-str :formal 5/11 :digits-after-dot 3)))
  (is (equal "壱円壱厘" (yen-str :formal 1001/1000 :digits-after-dot 3)))

  (is (equal "マイナス五拾銭" (yen-str :formal -1/2 :digits-after-dot 3)))
  (is (equal "マイナス参拾参銭参厘" (yen-str :formal -1/3 :digits-after-dot 3)))
  (is (equal "マイナス弐円五拾銭" (yen-str :formal -5/2 :digits-after-dot 3)))
  (is (equal "マイナス四拾五銭五厘" (yen-str :formal -5/11 :digits-after-dot 3)))

  ;; ratio 0
  ;; (is (equal "壱円" (yen-str :formal 1/2 :digits-after-dot 0))) ; 1/2 is ambiguous
  (is (equal "〇円" (yen-str :formal 1/3 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :formal 1/4 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :formal 1/5 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :formal 1/6 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :formal 1/7 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :formal 1/8 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :formal 1/9 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :formal 1/10 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :formal 1/100 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :formal 1/1000 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :formal 1/10000 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :formal 1/100000 :digits-after-dot 0)))
  (is (equal "壱円" (yen-str :formal 11/10 :digits-after-dot 0)))
  ;; (is (equal "参円" (yen-str :formal 5/2 :digits-after-dot 0))) ; 1/2 is ambiguous
  (is (equal "〇円" (yen-str :formal 5/11 :digits-after-dot 0)))
  (is (equal "壱円" (yen-str :formal 1001/1000 :digits-after-dot 0)))

  ;; (is (equal "マイナス壱円" (yen-str :formal -1/2 :digits-after-dot 0))) ; 1/2 is ambiguous
  (is (equal "〇円" (yen-str :formal -1/3 :digits-after-dot 0)))
  ;; (is (equal "マイナス参円" (yen-str :formal -5/2 :digits-after-dot 0))) ; 1/2 is ambiguous
  (is (equal "〇円" (yen-str :formal -5/11 :digits-after-dot 0)))

  ;; float 2
  (is (equal "拾銭" (yen-str :formal 0.1)))
  (is (equal "拾壱銭" (yen-str :formal 0.11)))
  (is (equal "拾弐銭" (yen-str :formal 0.123)))
  (is (equal "参拾壱銭" (yen-str :formal 0.306)))
  (is (equal "壱円弐拾銭" (yen-str :formal 1.2)))
  (is (equal "壱円参銭" (yen-str :formal 1.03)))
  (is (equal "壱銭" (yen-str :formal 0.006)))
  (is (equal "壱円壱銭" (yen-str :formal 1.006)))
  
  (is (equal "マイナス拾銭" (yen-str :formal -0.1)))
  (is (equal "マイナス壱円参銭" (yen-str :formal -1.03)))
  (is (equal "マイナス壱銭" (yen-str :formal -0.006)))
  (is (equal "マイナス壱円壱銭" (yen-str :formal -1.006)))

  ;; float 3
  (is (equal "拾銭" (yen-str :formal 0.1 :digits-after-dot 3)))
  (is (equal "拾壱銭" (yen-str :formal 0.11 :digits-after-dot 3)))
  (is (equal "拾弐銭参厘" (yen-str :formal 0.123 :digits-after-dot 3)))
  (is (equal "参拾銭六厘" (yen-str :formal 0.306 :digits-after-dot 3)))
  (is (equal "壱円弐拾銭" (yen-str :formal 1.2 :digits-after-dot 3)))
  (is (equal "壱円参銭" (yen-str :formal 1.03 :digits-after-dot 3)))
  (is (equal "六厘" (yen-str :formal 0.006 :digits-after-dot 3)))
  (is (equal "壱円六厘" (yen-str :formal 1.006 :digits-after-dot 3)))
  
  (is (equal "マイナス拾銭" (yen-str :formal -0.1 :digits-after-dot 3)))
  (is (equal "マイナス壱円参銭" (yen-str :formal -1.03 :digits-after-dot 3)))
  (is (equal "マイナス六厘" (yen-str :formal -0.006 :digits-after-dot 3)))
  (is (equal "マイナス壱円六厘" (yen-str :formal -1.006 :digits-after-dot 3)))

  ;; float 0
  (is (equal "〇円" (yen-str :formal 0.1 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :formal 0.11 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :formal 0.123 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :formal 0.306 :digits-after-dot 0)))
  (is (equal "壱円" (yen-str :formal 1.2 :digits-after-dot 0)))
  (is (equal "壱円" (yen-str :formal 1.03 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :formal 0.006 :digits-after-dot 0)))
  (is (equal "壱円" (yen-str :formal 1.006 :digits-after-dot 0)))
  
  (is (equal "〇円" (yen-str :formal -0.1 :digits-after-dot 0)))
  (is (equal "マイナス壱円" (yen-str :formal -1.03 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :formal -0.006 :digits-after-dot 0)))
  (is (equal "マイナス壱円" (yen-str :formal -1.006 :digits-after-dot 0)))
  ;; 
  t)

(test test-yen-old
  ;; integer
  (is (equal "零圓" (yen-str :old 0)))
  (is (equal "壹圓" (yen-str :old 1)))
  (is (equal "貳圓" (yen-str :old 2)))
  (is (equal "拾圓" (yen-str :old 10)))
  (is (equal "拾壹圓" (yen-str :old 11)))
  (is (equal "拾貳圓" (yen-str :old 12)))
  (is (equal "拾玖圓" (yen-str :old 19)))
  (is (equal "貳拾圓" (yen-str :old 20)))
  (is (equal "貳拾壹圓" (yen-str :old 21)))
  (is (equal "貳拾貳圓" (yen-str :old 22)))
  (is (equal "壹佰圓" (yen-str :old 100)))
  (is (equal "貳佰壹圓" (yen-str :old 201)))
  (is (equal "壹仟圓" (yen-str :old 1000)))
  (is (equal "壹仟壹佰拾壹圓" (yen-str :old 1111)))
  (is (equal "貳仟貳佰貳拾貳圓" (yen-str :old 2222)))
  (is (equal "壹萬圓" (yen-str :old 10000)))
  (is (equal "壹佰貳拾壹萬圓" (yen-str :old 1210000)))
  (is (equal "壹仟萬圓" (yen-str :old 10000000)))
  (is (equal "壹仟貳拾萬參仟肆拾圓" (yen-str :old 10203040)))

  (is (equal "負之壹圓" (yen-str :old -1)))
  (is (equal "負之貳圓" (yen-str :old -2)))

  ;; ratio 2
  (is (equal "伍拾錢" (yen-str :old 1/2)))
  (is (equal "參拾參錢" (yen-str :old 1/3)))
  (is (equal "貳拾伍錢" (yen-str :old 1/4)))
  (is (equal "貳拾錢" (yen-str :old 1/5)))
  (is (equal "拾柒錢" (yen-str :old 1/6)))
  (is (equal "拾肆錢" (yen-str :old 1/7)))
  (is (equal "拾貳錢" (yen-str :old 1/8)))
  (is (equal "拾壹錢" (yen-str :old 1/9)))
  (is (equal "拾錢" (yen-str :old 1/10)))
  (is (equal "壹錢" (yen-str :old 1/100)))
  (is (equal "零圓" (yen-str :old 1/1000)))
  (is (equal "零圓" (yen-str :old 1/10000)))
  (is (equal "零圓" (yen-str :old 1/100000)))
  (is (equal "壹圓拾錢" (yen-str :old 11/10)))
  (is (equal "貳圓伍拾錢" (yen-str :old 5/2)))
  (is (equal "肆拾伍錢" (yen-str :old 5/11)))
  (is (equal "壹圓" (yen-str :old 1001/1000)))

  (is (equal "負之伍拾錢" (yen-str :old -1/2)))
  (is (equal "負之參拾參錢" (yen-str :old -1/3)))
  (is (equal "負之貳圓伍拾錢" (yen-str :old -5/2)))
  (is (equal "負之肆拾伍錢" (yen-str :old -5/11)))

  ;; ratio 3
  (is (equal "伍拾錢" (yen-str :old 1/2 :digits-after-dot 3)))
  (is (equal "參拾參錢參釐" (yen-str :old 1/3 :digits-after-dot 3)))
  (is (equal "貳拾伍錢" (yen-str :old 1/4 :digits-after-dot 3)))
  (is (equal "貳拾錢" (yen-str :old 1/5 :digits-after-dot 3)))
  (is (equal "拾陸錢柒釐" (yen-str :old 1/6 :digits-after-dot 3)))
  (is (equal "拾肆錢參釐" (yen-str :old 1/7 :digits-after-dot 3)))
  (is (equal "拾貳錢伍釐" (yen-str :old 1/8 :digits-after-dot 3)))
  (is (equal "拾壹錢壹釐" (yen-str :old 1/9 :digits-after-dot 3)))
  (is (equal "拾錢" (yen-str :old 1/10 :digits-after-dot 3)))
  (is (equal "壹錢" (yen-str :old 1/100 :digits-after-dot 3)))
  (is (equal "壹釐" (yen-str :old 1/1000 :digits-after-dot 3)))
  (is (equal "零圓" (yen-str :old 1/10000 :digits-after-dot 3)))
  (is (equal "零圓" (yen-str :old 1/100000 :digits-after-dot 3)))
  (is (equal "壹圓拾錢" (yen-str :old 11/10 :digits-after-dot 3)))
  (is (equal "貳圓伍拾錢" (yen-str :old 5/2 :digits-after-dot 3)))
  (is (equal "肆拾伍錢伍釐" (yen-str :old 5/11 :digits-after-dot 3)))
  (is (equal "壹圓壹釐" (yen-str :old 1001/1000 :digits-after-dot 3)))

  (is (equal "負之伍拾錢" (yen-str :old -1/2 :digits-after-dot 3)))
  (is (equal "負之參拾參錢參釐" (yen-str :old -1/3 :digits-after-dot 3)))
  (is (equal "負之貳圓伍拾錢" (yen-str :old -5/2 :digits-after-dot 3)))
  (is (equal "負之肆拾伍錢伍釐" (yen-str :old -5/11 :digits-after-dot 3)))

  ;; ratio 0
  ;; (is (equal "壹圓" (yen-str :old 1/2 :digits-after-dot 0))) ; 1/2 is ambiguous
  (is (equal "零圓" (yen-str :old 1/3 :digits-after-dot 0)))
  (is (equal "零圓" (yen-str :old 1/4 :digits-after-dot 0)))
  (is (equal "零圓" (yen-str :old 1/5 :digits-after-dot 0)))
  (is (equal "零圓" (yen-str :old 1/6 :digits-after-dot 0)))
  (is (equal "零圓" (yen-str :old 1/7 :digits-after-dot 0)))
  (is (equal "零圓" (yen-str :old 1/8 :digits-after-dot 0)))
  (is (equal "零圓" (yen-str :old 1/9 :digits-after-dot 0)))
  (is (equal "零圓" (yen-str :old 1/10 :digits-after-dot 0)))
  (is (equal "零圓" (yen-str :old 1/100 :digits-after-dot 0)))
  (is (equal "零圓" (yen-str :old 1/1000 :digits-after-dot 0)))
  (is (equal "零圓" (yen-str :old 1/10000 :digits-after-dot 0)))
  (is (equal "零圓" (yen-str :old 1/100000 :digits-after-dot 0)))
  (is (equal "壹圓" (yen-str :old 11/10 :digits-after-dot 0)))
  ;; (is (equal "參圓" (yen-str :old 5/2 :digits-after-dot 0))) ; 1/2 is ambiguous
  (is (equal "零圓" (yen-str :old 5/11 :digits-after-dot 0)))
  (is (equal "壹圓" (yen-str :old 1001/1000 :digits-after-dot 0)))

  ;; (is (equal "負之壹圓" (yen-str :old -1/2 :digits-after-dot 0))) ; 1/2 is ambiguous
  (is (equal "零圓" (yen-str :old -1/3 :digits-after-dot 0)))
  ;; (is (equal "負之參圓" (yen-str :old -5/2 :digits-after-dot 0))) ; 1/2 is ambiguous
  (is (equal "零圓" (yen-str :old -5/11 :digits-after-dot 0)))

  ;; float 2
  (is (equal "拾錢" (yen-str :old 0.1)))
  (is (equal "拾壹錢" (yen-str :old 0.11)))
  (is (equal "拾貳錢" (yen-str :old 0.123)))
  (is (equal "參拾壹錢" (yen-str :old 0.306)))
  (is (equal "壹圓貳拾錢" (yen-str :old 1.2)))
  (is (equal "壹圓參錢" (yen-str :old 1.03)))
  (is (equal "壹錢" (yen-str :old 0.006)))
  (is (equal "壹圓壹錢" (yen-str :old 1.006)))
  
  (is (equal "負之拾錢" (yen-str :old -0.1)))
  (is (equal "負之壹圓參錢" (yen-str :old -1.03)))
  (is (equal "負之壹錢" (yen-str :old -0.006)))
  (is (equal "負之壹圓壹錢" (yen-str :old -1.006)))

  ;; float 3
  (is (equal "拾錢" (yen-str :old 0.1 :digits-after-dot 3)))
  (is (equal "拾壹錢" (yen-str :old 0.11 :digits-after-dot 3)))
  (is (equal "拾貳錢參釐" (yen-str :old 0.123 :digits-after-dot 3)))
  (is (equal "參拾錢陸釐" (yen-str :old 0.306 :digits-after-dot 3)))
  (is (equal "壹圓貳拾錢" (yen-str :old 1.2 :digits-after-dot 3)))
  (is (equal "壹圓參錢" (yen-str :old 1.03 :digits-after-dot 3)))
  (is (equal "陸釐" (yen-str :old 0.006 :digits-after-dot 3)))
  (is (equal "壹圓陸釐" (yen-str :old 1.006 :digits-after-dot 3)))
  
  (is (equal "負之拾錢" (yen-str :old -0.1 :digits-after-dot 3)))
  (is (equal "負之壹圓參錢" (yen-str :old -1.03 :digits-after-dot 3)))
  (is (equal "負之陸釐" (yen-str :old -0.006 :digits-after-dot 3)))
  (is (equal "負之壹圓陸釐" (yen-str :old -1.006 :digits-after-dot 3)))

  ;; float 0
  (is (equal "零圓" (yen-str :old 0.1 :digits-after-dot 0)))
  (is (equal "零圓" (yen-str :old 0.11 :digits-after-dot 0)))
  (is (equal "零圓" (yen-str :old 0.123 :digits-after-dot 0)))
  (is (equal "零圓" (yen-str :old 0.306 :digits-after-dot 0)))
  (is (equal "壹圓" (yen-str :old 1.2 :digits-after-dot 0)))
  (is (equal "壹圓" (yen-str :old 1.03 :digits-after-dot 0)))
  (is (equal "零圓" (yen-str :old 0.006 :digits-after-dot 0)))
  (is (equal "壹圓" (yen-str :old 1.006 :digits-after-dot 0)))
  
  (is (equal "零圓" (yen-str :old -0.1 :digits-after-dot 0)))
  (is (equal "負之壹圓" (yen-str :old -1.03 :digits-after-dot 0)))
  (is (equal "零圓" (yen-str :old -0.006 :digits-after-dot 0)))
  (is (equal "負之壹圓" (yen-str :old -1.006 :digits-after-dot 0)))
  ;; 
  t)

(test test-yen-positional
  ;; integer
  (is (equal "〇円" (yen-str :positional 0)))
  (is (equal "一円" (yen-str :positional 1)))
  (is (equal "二円" (yen-str :positional 2)))
  (is (equal "一〇円" (yen-str :positional 10)))
  (is (equal "一一円" (yen-str :positional 11)))
  (is (equal "一二円" (yen-str :positional 12)))
  (is (equal "一九円" (yen-str :positional 19)))
  (is (equal "二〇円" (yen-str :positional 20)))
  (is (equal "二一円" (yen-str :positional 21)))
  (is (equal "二二円" (yen-str :positional 22)))
  (is (equal "一〇〇円" (yen-str :positional 100)))
  (is (equal "二〇一円" (yen-str :positional 201)))
  (is (equal "一〇〇〇円" (yen-str :positional 1000)))
  (is (equal "一一一一円" (yen-str :positional 1111)))
  (is (equal "二二二二円" (yen-str :positional 2222)))
  (is (equal "一〇〇〇〇円" (yen-str :positional 10000)))
  (is (equal "一二一〇〇〇〇円" (yen-str :positional 1210000)))
  (is (equal "一〇〇〇〇〇〇〇円" (yen-str :positional 10000000)))
  (is (equal "一〇二〇三〇四〇円" (yen-str :positional 10203040)))

  (is (equal "−一円" (yen-str :positional -1)))
  (is (equal "−二円" (yen-str :positional -2)))

  ;; ratio 2
  (is (equal "五〇銭" (yen-str :positional 1/2)))
  (is (equal "三三銭" (yen-str :positional 1/3)))
  (is (equal "二五銭" (yen-str :positional 1/4)))
  (is (equal "二〇銭" (yen-str :positional 1/5)))
  (is (equal "一七銭" (yen-str :positional 1/6)))
  (is (equal "一四銭" (yen-str :positional 1/7)))
  (is (equal "一二銭" (yen-str :positional 1/8)))
  (is (equal "一一銭" (yen-str :positional 1/9)))
  (is (equal "一〇銭" (yen-str :positional 1/10)))
  (is (equal "一銭" (yen-str :positional 1/100)))
  (is (equal "〇円" (yen-str :positional 1/1000)))
  (is (equal "〇円" (yen-str :positional 1/10000)))
  (is (equal "〇円" (yen-str :positional 1/100000)))
  (is (equal "一円一〇銭" (yen-str :positional 11/10)))
  (is (equal "二円五〇銭" (yen-str :positional 5/2)))
  (is (equal "四五銭" (yen-str :positional 5/11)))
  (is (equal "一円" (yen-str :positional 1001/1000)))

  (is (equal "−五〇銭" (yen-str :positional -1/2)))
  (is (equal "−三三銭" (yen-str :positional -1/3)))
  (is (equal "−二円五〇銭" (yen-str :positional -5/2)))
  (is (equal "−四五銭" (yen-str :positional -5/11)))

  ;; ratio 3
  (is (equal "五〇銭" (yen-str :positional 1/2 :digits-after-dot 3)))
  (is (equal "三三銭三厘" (yen-str :positional 1/3 :digits-after-dot 3)))
  (is (equal "二五銭" (yen-str :positional 1/4 :digits-after-dot 3)))
  (is (equal "二〇銭" (yen-str :positional 1/5 :digits-after-dot 3)))
  (is (equal "一六銭七厘" (yen-str :positional 1/6 :digits-after-dot 3)))
  (is (equal "一四銭三厘" (yen-str :positional 1/7 :digits-after-dot 3)))
  (is (equal "一二銭五厘" (yen-str :positional 1/8 :digits-after-dot 3)))
  (is (equal "一一銭一厘" (yen-str :positional 1/9 :digits-after-dot 3)))
  (is (equal "一〇銭" (yen-str :positional 1/10 :digits-after-dot 3)))
  (is (equal "一銭" (yen-str :positional 1/100 :digits-after-dot 3)))
  (is (equal "一厘" (yen-str :positional 1/1000 :digits-after-dot 3)))
  (is (equal "〇円" (yen-str :positional 1/10000 :digits-after-dot 3)))
  (is (equal "〇円" (yen-str :positional 1/100000 :digits-after-dot 3)))
  (is (equal "一円一〇銭" (yen-str :positional 11/10 :digits-after-dot 3)))
  (is (equal "二円五〇銭" (yen-str :positional 5/2 :digits-after-dot 3)))
  (is (equal "四五銭五厘" (yen-str :positional 5/11 :digits-after-dot 3)))
  (is (equal "一円一厘" (yen-str :positional 1001/1000 :digits-after-dot 3)))

  (is (equal "−五〇銭" (yen-str :positional -1/2 :digits-after-dot 3)))
  (is (equal "−三三銭三厘" (yen-str :positional -1/3 :digits-after-dot 3)))
  (is (equal "−二円五〇銭" (yen-str :positional -5/2 :digits-after-dot 3)))
  (is (equal "−四五銭五厘" (yen-str :positional -5/11 :digits-after-dot 3)))

  ;; ratio 0
  ;; (is (equal "一円" (yen-str :positional 1/2 :digits-after-dot 0))) ; 1/2 is ambiguous
  (is (equal "〇円" (yen-str :positional 1/3 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :positional 1/4 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :positional 1/5 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :positional 1/6 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :positional 1/7 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :positional 1/8 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :positional 1/9 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :positional 1/10 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :positional 1/100 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :positional 1/1000 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :positional 1/10000 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :positional 1/100000 :digits-after-dot 0)))
  (is (equal "一円" (yen-str :positional 11/10 :digits-after-dot 0)))
  ;; (is (equal "三円" (yen-str :positional 5/2 :digits-after-dot 0))) ; 1/2 is ambiguous
  (is (equal "〇円" (yen-str :positional 5/11 :digits-after-dot 0)))
  (is (equal "一円" (yen-str :positional 1001/1000 :digits-after-dot 0)))

  ;; (is (equal "−一円" (yen-str :positional -1/2 :digits-after-dot 0))) ; 1/2 is ambiguous
  (is (equal "〇円" (yen-str :positional -1/3 :digits-after-dot 0)))
  ;; (is (equal "−三円" (yen-str :positional -5/2 :digits-after-dot 0))) ; 1/2 is ambiguous
  (is (equal "〇円" (yen-str :positional -5/11 :digits-after-dot 0)))

  ;; float 2
  (is (equal "一〇銭" (yen-str :positional 0.1)))
  (is (equal "一一銭" (yen-str :positional 0.11)))
  (is (equal "一二銭" (yen-str :positional 0.123)))
  (is (equal "三一銭" (yen-str :positional 0.306)))
  (is (equal "一円二〇銭" (yen-str :positional 1.2)))
  (is (equal "一円三銭" (yen-str :positional 1.03)))
  (is (equal "一銭" (yen-str :positional 0.006)))
  (is (equal "一円一銭" (yen-str :positional 1.006)))
  
  (is (equal "−一〇銭" (yen-str :positional -0.1)))
  (is (equal "−一円三銭" (yen-str :positional -1.03)))
  (is (equal "−一銭" (yen-str :positional -0.006)))
  (is (equal "−一円一銭" (yen-str :positional -1.006)))

  ;; float 3
  (is (equal "一〇銭" (yen-str :positional 0.1 :digits-after-dot 3)))
  (is (equal "一一銭" (yen-str :positional 0.11 :digits-after-dot 3)))
  (is (equal "一二銭三厘" (yen-str :positional 0.123 :digits-after-dot 3)))
  (is (equal "三〇銭六厘" (yen-str :positional 0.306 :digits-after-dot 3)))
  (is (equal "一円二〇銭" (yen-str :positional 1.2 :digits-after-dot 3)))
  (is (equal "一円三銭" (yen-str :positional 1.03 :digits-after-dot 3)))
  (is (equal "六厘" (yen-str :positional 0.006 :digits-after-dot 3)))
  (is (equal "一円六厘" (yen-str :positional 1.006 :digits-after-dot 3)))
  
  (is (equal "−一〇銭" (yen-str :positional -0.1 :digits-after-dot 3)))
  (is (equal "−一円三銭" (yen-str :positional -1.03 :digits-after-dot 3)))
  (is (equal "−六厘" (yen-str :positional -0.006 :digits-after-dot 3)))
  (is (equal "−一円六厘" (yen-str :positional -1.006 :digits-after-dot 3)))

  ;; float 0
  (is (equal "〇円" (yen-str :positional 0.1 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :positional 0.11 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :positional 0.123 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :positional 0.306 :digits-after-dot 0)))
  (is (equal "一円" (yen-str :positional 1.2 :digits-after-dot 0)))
  (is (equal "一円" (yen-str :positional 1.03 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :positional 0.006 :digits-after-dot 0)))
  (is (equal "一円" (yen-str :positional 1.006 :digits-after-dot 0)))
  
  (is (equal "〇円" (yen-str :positional -0.1 :digits-after-dot 0)))
  (is (equal "−一円" (yen-str :positional -1.03 :digits-after-dot 0)))
  (is (equal "〇円" (yen-str :positional -0.006 :digits-after-dot 0)))
  (is (equal "−一円" (yen-str :positional -1.006 :digits-after-dot 0)))
  ;; 
  t)

(test test-yen-fallback
  ;; too big num
  (is (equal "一無量大数円" (yen-str :normal (expt 10 68))))
  (is (equal "一〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇円"
	     (yen-str :normal (expt 10 72))))
  (is (equal "マイナス一無量大数円" (yen-str :normal (- (expt 10 68)))))
  (is (equal "−一〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇〇円"
  	     (yen-str :normal (- (expt 10 72)))))
  ;; ;; too small num
  (is (equal "〇円" (yen-str :normal (float (expt 10 -21)))))
  (is (equal "〇円" (yen-str :normal (float (expt 10 -22)))))
  (is (equal "〇円" (yen-str :normal (float (expt 10 -23)))))
  (is (equal "〇円" (yen-str :normal (- (float (expt 10 -21))))))
  (is (equal "〇円" (yen-str :normal (- (float (expt 10 -22))))))
  (is (equal "〇円" (yen-str :normal (- (float (expt 10 -23))))))
  ;; complex
  (signals error (yen-str :normal #c(1 1)))
  ;; 
  t)

(test test-yen-parameters
  ;; all has been done above..
  t)
