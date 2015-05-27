(IN-PACKAGE "JP-NUMERAL")

(DEFCONSTANT +TABLE-NORMAL-INDEX+ 0)
(DEFCONSTANT +TABLE-FORMAL-INDEX+ 1)
(DEFCONSTANT +TABLE-OLD-INDEX+ 2)
(DEFCONSTANT +TABLE-POSITIONAL-INDEX+ 3)

(DEFCONSTANT +DIGITS+
    (QUOTE
     #(#(#1=#.(BABEL:OCTETS-TO-STRING
                 (MAKE-ARRAY '(3)
                   :INITIAL-CONTENTS '(227 128 135)
                   . #2=(:ELEMENT-TYPE '(UNSIGNED-BYTE 8)))
                . #3=(:ENCODING :UTF-8))
         #1#
         #.(BABEL:OCTETS-TO-STRING
              (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(233 155 182) . #2#)
             . #3#))
       #(#.(BABEL:OCTETS-TO-STRING
              (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(228 184 128) . #2#)
             . #3#)
         #.(BABEL:OCTETS-TO-STRING
              (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(229 163 177) . #2#)
             . #3#)
         #.(BABEL:OCTETS-TO-STRING
              (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(229 163 185) . #2#)
             . #3#))
       #(#.(BABEL:OCTETS-TO-STRING
              (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(228 186 140) . #2#)
             . #3#)
         #.(BABEL:OCTETS-TO-STRING
              (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(229 188 144) . #2#)
             . #3#)
         #.(BABEL:OCTETS-TO-STRING
              (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(232 178 179) . #2#)
             . #3#))
       #(#.(BABEL:OCTETS-TO-STRING
              (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(228 184 137) . #2#)
             . #3#)
         #.(BABEL:OCTETS-TO-STRING
              (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(229 143 130) . #2#)
             . #3#)
         #.(BABEL:OCTETS-TO-STRING
              (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(229 143 131) . #2#)
             . #3#))
       #(#4=#.(BABEL:OCTETS-TO-STRING
                 (MAKE-ARRAY '(3)
                   :INITIAL-CONTENTS '(229 155 155)
                   . #2#)
                . #3#)
         #4#
         #.(BABEL:OCTETS-TO-STRING
              (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(232 130 134) . #2#)
             . #3#))
       #(#5=#.(BABEL:OCTETS-TO-STRING
                 (MAKE-ARRAY '(3)
                   :INITIAL-CONTENTS '(228 186 148)
                   . #2#)
                . #3#)
         #5#
         #.(BABEL:OCTETS-TO-STRING
              (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(228 188 141) . #2#)
             . #3#))
       #(#6=#.(BABEL:OCTETS-TO-STRING
                 (MAKE-ARRAY '(3)
                   :INITIAL-CONTENTS '(229 133 173)
                   . #2#)
                . #3#)
         #6#
         #.(BABEL:OCTETS-TO-STRING
              (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(233 153 184) . #2#)
             . #3#))
       #(#7=#.(BABEL:OCTETS-TO-STRING
                 (MAKE-ARRAY '(3)
                   :INITIAL-CONTENTS '(228 184 131)
                   . #2#)
                . #3#)
         #7#
         #.(BABEL:OCTETS-TO-STRING
              (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(230 159 146) . #2#)
             . #3#))
       #(#8=#.(BABEL:OCTETS-TO-STRING
                 (MAKE-ARRAY '(3)
                   :INITIAL-CONTENTS '(229 133 171)
                   . #2#)
                . #3#)
         #8#
         #.(BABEL:OCTETS-TO-STRING
              (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(230 141 140) . #2#)
             . #3#))
       #(#9=#.(BABEL:OCTETS-TO-STRING
                 (MAKE-ARRAY '(3)
                   :INITIAL-CONTENTS '(228 185 157)
                   . #2#)
                . #3#)
         #9#
         #.(BABEL:OCTETS-TO-STRING
              (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(231 142 150) . #2#)
             . #3#))))
  "A vector of (<normal> <formal> <old>)")

(DEFCONSTANT +POWER-ALIST+
    (QUOTE
     ((0
       . #(#1=#.(BABEL:OCTETS-TO-STRING
                   (MAKE-ARRAY '(0)
                     :INITIAL-CONTENTS 'NIL
                     . #2=(:ELEMENT-TYPE '(UNSIGNED-BYTE 8)))
                  . #3=(:ENCODING :UTF-8))
           #1# #1#))
      (1
       . #(#.(BABEL:OCTETS-TO-STRING
                (MAKE-ARRAY '(3)
                  :INITIAL-CONTENTS '(229 141 129)
                  . #2#)
               . #3#)
           #4=#.(BABEL:OCTETS-TO-STRING
                   (MAKE-ARRAY '(3)
                     :INITIAL-CONTENTS '(230 139 190)
                     . #2#)
                  . #3#)
           #4#))
      (2
       . #(#5=#.(BABEL:OCTETS-TO-STRING
                   (MAKE-ARRAY '(3)
                     :INITIAL-CONTENTS '(231 153 190)
                     . #2#)
                  . #3#)
           #5#
           #.(BABEL:OCTETS-TO-STRING
                (MAKE-ARRAY '(3)
                  :INITIAL-CONTENTS '(228 189 176)
                  . #2#)
               . #3#)))
      (3
       . #(#6=#.(BABEL:OCTETS-TO-STRING
                   (MAKE-ARRAY '(3)
                     :INITIAL-CONTENTS '(229 141 131)
                     . #2#)
                  . #3#)
           #6#
           #.(BABEL:OCTETS-TO-STRING
                (MAKE-ARRAY '(3)
                  :INITIAL-CONTENTS '(228 187 159)
                  . #2#)
               . #3#)))
      (4
       . #(#7=#.(BABEL:OCTETS-TO-STRING
                   (MAKE-ARRAY '(3)
                     :INITIAL-CONTENTS '(228 184 135)
                     . #2#)
                  . #3#)
           #7#
           #.(BABEL:OCTETS-TO-STRING
                (MAKE-ARRAY '(3)
                  :INITIAL-CONTENTS '(232 144 172)
                  . #2#)
               . #3#)))
      (8
       . #(#8=#.(BABEL:OCTETS-TO-STRING
                   (MAKE-ARRAY '(3)
                     :INITIAL-CONTENTS '(229 132 132)
                     . #2#)
                  . #3#)
           #8# #8#))
      (12
       . #(#9=#.(BABEL:OCTETS-TO-STRING
                   (MAKE-ARRAY '(3)
                     :INITIAL-CONTENTS '(229 133 134)
                     . #2#)
                  . #3#)
           #9# #9#))
      (16
       . #(#10=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(3)
                      :INITIAL-CONTENTS '(228 186 172)
                      . #2#)
                   . #3#)
           #10# #10#))
      (20
       . #(#11=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(3)
                      :INITIAL-CONTENTS '(229 158 147)
                      . #2#)
                   . #3#)
           #11# #11#))
      (24
       . #(#12=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(4)
                      :INITIAL-CONTENTS '(240 165 157 177)
                      . #2#)
                   . #3#)
           #12#
           #.(BABEL:OCTETS-TO-STRING
                (MAKE-ARRAY '(3)
                  :INITIAL-CONTENTS '(231 167 173)
                  . #2#)
               . #3#)))
      (28
       . #(#13=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(3)
                      :INITIAL-CONTENTS '(231 169 163)
                      . #2#)
                   . #3#)
           #13# #13#))
      (32
       . #(#14=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(3)
                      :INITIAL-CONTENTS '(230 186 157)
                      . #2#)
                   . #3#)
           #14# #14#))
      (36
       . #(#15=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(3)
                      :INITIAL-CONTENTS '(230 190 151)
                      . #2#)
                   . #3#)
           #15# #15#))
      (40
       . #(#16=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(3)
                      :INITIAL-CONTENTS '(230 173 163)
                      . #2#)
                   . #3#)
           #16# #16#))
      (44
       . #(#17=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(3)
                      :INITIAL-CONTENTS '(232 188 137)
                      . #2#)
                   . #3#)
           #17# #17#))
      (48
       . #(#18=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(3)
                      :INITIAL-CONTENTS '(230 165 181)
                      . #2#)
                   . #3#)
           #18# #18#))
      (52
       . #(#19=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(9)
                      :INITIAL-CONTENTS '(230
                                          129
                                          146
                                          230
                                          178
                                          179
                                          230
                                          178
                                          153)
                      . #2#)
                   . #3#)
           #19# #19#))
      (56
       . #(#20=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(9)
                      :INITIAL-CONTENTS '(233
                                          152
                                          191
                                          229
                                          131
                                          167
                                          231
                                          165
                                          135)
                      . #2#)
                   . #3#)
           #20# #20#))
      (60
       . #(#21=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(9)
                      :INITIAL-CONTENTS '(233
                                          130
                                          163
                                          231
                                          148
                                          177
                                          228
                                          187
                                          150)
                      . #2#)
                   . #3#)
           #21# #21#))
      (64
       . #(#22=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(12)
                      :INITIAL-CONTENTS '(228
                                          184
                                          141
                                          229
                                          143
                                          175
                                          230
                                          128
                                          157
                                          232
                                          173
                                          176)
                      . #2#)
                   . #3#)
           #22# #22#))
      (68
       . #(#23=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(12)
                      :INITIAL-CONTENTS '(231
                                          132
                                          161
                                          233
                                          135
                                          143
                                          229
                                          164
                                          167
                                          230
                                          149
                                          176)
                      . #2#)
                   . #3#)
           #23# #23#))
      (-1
       . #(#24=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(3)
                      :INITIAL-CONTENTS '(229 136 134)
                      . #2#)
                   . #3#)
           #24# #24#))
      (-2
       . #(#25=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(3)
                      :INITIAL-CONTENTS '(229 142 152)
                      . #2#)
                   . #3#)
           #25#
           #.(BABEL:OCTETS-TO-STRING
                (MAKE-ARRAY '(3)
                  :INITIAL-CONTENTS '(233 135 144)
                  . #2#)
               . #3#)))
      (-3
       . #(#26=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(3)
                      :INITIAL-CONTENTS '(230 175 155)
                      . #2#)
                   . #3#)
           #26#
           #.(BABEL:OCTETS-TO-STRING
                (MAKE-ARRAY '(3)
                  :INITIAL-CONTENTS '(230 175 171)
                  . #2#)
               . #3#)))
      (-4
       . #(#27=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(3)
                      :INITIAL-CONTENTS '(231 179 184)
                      . #2#)
                   . #3#)
           #27#
           #.(BABEL:OCTETS-TO-STRING
                (MAKE-ARRAY '(3)
                  :INITIAL-CONTENTS '(231 181 178)
                  . #2#)
               . #3#)))
      (-5
       . #(#28=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(3)
                      :INITIAL-CONTENTS '(229 191 189)
                      . #2#)
                   . #3#)
           #28# #28#))
      (-6
       . #(#29=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(3)
                      :INITIAL-CONTENTS '(229 190 174)
                      . #2#)
                   . #3#)
           #29# #29#))
      (-7
       . #(#30=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(3)
                      :INITIAL-CONTENTS '(231 185 138)
                      . #2#)
                   . #3#)
           #30# #30#))
      (-8
       . #(#31=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(3)
                      :INITIAL-CONTENTS '(230 178 153)
                      . #2#)
                   . #3#)
           #31# #31#))
      (-9
       . #(#32=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(3)
                      :INITIAL-CONTENTS '(229 161 181)
                      . #2#)
                   . #3#)
           #32# #32#))
      (-10
       . #(#33=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(3)
                      :INITIAL-CONTENTS '(229 159 131)
                      . #2#)
                   . #3#)
           #33# #33#))
      (-11
       . #(#34=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(3)
                      :INITIAL-CONTENTS '(230 184 186)
                      . #2#)
                   . #3#)
           #34# #34#))
      (-12
       . #(#35=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(3)
                      :INITIAL-CONTENTS '(230 188 160)
                      . #2#)
                   . #3#)
           #35# #35#))
      (-13
       . #(#36=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(6)
                      :INITIAL-CONTENTS '(230 168 161 231 179 138)
                      . #2#)
                   . #3#)
           #36# #36#))
      (-14
       . #(#37=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(6)
                      :INITIAL-CONTENTS '(233 128 161 229 183 161)
                      . #2#)
                   . #3#)
           #37# #37#))
      (-15
       . #(#38=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(6)
                      :INITIAL-CONTENTS '(233 160 136 232 135 190)
                      . #2#)
                   . #3#)
           #38# #38#))
      (-16
       . #(#39=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(6)
                      :INITIAL-CONTENTS '(231 158 172 230 129 175)
                      . #2#)
                   . #3#)
           #39# #39#))
      (-17
       . #(#40=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(6)
                      :INITIAL-CONTENTS '(229 188 190 230 140 135)
                      . #2#)
                   . #3#)
           #40# #40#))
      (-18
       . #(#41=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(6)
                      :INITIAL-CONTENTS '(229 136 185 233 130 163)
                      . #2#)
                   . #3#)
           #41# #41#))
      (-19
       . #(#42=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(6)
                      :INITIAL-CONTENTS '(229 133 173 229 190 179)
                      . #2#)
                   . #3#)
           #42# #42#))
      (-20
       . #(#43=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(6)
                      :INITIAL-CONTENTS '(232 153 154 231 169 186)
                      . #2#)
                   . #3#)
           #43# #43#))
      (-21
       . #(#44=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(6)
                      :INITIAL-CONTENTS '(230 184 133 230 181 132)
                      . #2#)
                   . #3#)
           #44# #44#))))
  "An alist of (<power> . (<normal> <formal> <old>))")

(DEFCONSTANT +POWER-MAX+ 68)

(DEFCONSTANT +MINUS-SIGN+
    (QUOTE
     #(#.(BABEL:OCTETS-TO-STRING
            (MAKE-ARRAY '(12)
              :INITIAL-CONTENTS '(227
                                  131
                                  158
                                  227
                                  130
                                  164
                                  227
                                  131
                                  138
                                  227
                                  130
                                  185)
              . #1=(:ELEMENT-TYPE '(UNSIGNED-BYTE 8)))
           . #2=(:ENCODING :UTF-8))
       #.(BABEL:OCTETS-TO-STRING
            (MAKE-ARRAY '(6)
              :INITIAL-CONTENTS '(232 178 160 227 129 174)
              . #1#)
           . #2#)
       #.(BABEL:OCTETS-TO-STRING
            (MAKE-ARRAY '(6)
              :INITIAL-CONTENTS '(232 178 160 228 185 139)
              . #1#)
           . #2#)
       #.(BABEL:OCTETS-TO-STRING
            (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(226 136 146) . #1#)
           . #2#)))
  "A vector of (<normal> <formal> <old> <positional>")

(DEFCONSTANT +FRACTION-PARTS-OF+
    (QUOTE
     #(#1=#.(BABEL:OCTETS-TO-STRING
               (MAKE-ARRAY '(6)
                 :INITIAL-CONTENTS '(229 136 134 227 129 174)
                 . #2=(:ELEMENT-TYPE '(UNSIGNED-BYTE 8)))
              . #3=(:ENCODING :UTF-8))
       #1#
       #.(BABEL:OCTETS-TO-STRING
            (MAKE-ARRAY '(6)
              :INITIAL-CONTENTS '(229 136 134 228 185 139)
              . #2#)
           . #3#)
       #.(BABEL:OCTETS-TO-STRING
            (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(239 188 143) . #2#)
           . #3#)))
  "A vector of (<normal> <formal> <old> <positional>")

(DEFCONSTANT +RADIX-POINT+
    (QUOTE
     #(#1=#.(BABEL:OCTETS-TO-STRING
               (MAKE-ARRAY '(3)
                 :INITIAL-CONTENTS '(227 131 187)
                 :ELEMENT-TYPE '(UNSIGNED-BYTE 8))
              :ENCODING :UTF-8)
       #1# #1# #1#))
  "A vector of (<normal> <formal> <old> <positional>")
