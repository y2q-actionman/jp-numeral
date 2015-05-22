(IN-PACKAGE "CL-JP-NUMERAL")

(DEFCONSTANT +JP-NUMERAL-TABLE-NORMAL-INDEX+ 0)
(DEFCONSTANT +JP-NUMERAL-TABLE-FINANCIAL-INDEX+ 1)
(DEFCONSTANT +JP-NUMERAL-TABLE-OLD-INDEX+ 2)
(DEFCONSTANT +JP-NUMERAL-TABLE-ALTERNATIVE-IN-BMP-INDEX+ 3)

(DEFCONSTANT +JP-NUMERAL-DECIMAL-ALIST+
    (QUOTE
     ((0
       . #(#1=#.(BABEL:OCTETS-TO-STRING
                   (MAKE-ARRAY '(3)
                     :ELEMENT-TYPE #2='(UNSIGNED-BYTE 8)
                     :INITIAL-CONTENTS '(227 128 135))
                  . #3=(:ENCODING :UTF-8))
           #1#
           #.(BABEL:OCTETS-TO-STRING
                (MAKE-ARRAY '(3)
                  :ELEMENT-TYPE #2#
                  :INITIAL-CONTENTS '(233 155 182))
               . #3#)
           NIL))
      (1
       . #(#.(BABEL:OCTETS-TO-STRING
                (MAKE-ARRAY '(3)
                  :ELEMENT-TYPE #2#
                  :INITIAL-CONTENTS '(228 184 128))
               . #3#)
           #.(BABEL:OCTETS-TO-STRING
                (MAKE-ARRAY '(3)
                  :ELEMENT-TYPE #2#
                  :INITIAL-CONTENTS '(229 163 177))
               . #3#)
           #.(BABEL:OCTETS-TO-STRING
                (MAKE-ARRAY '(3)
                  :ELEMENT-TYPE #2#
                  :INITIAL-CONTENTS '(229 163 185))
               . #3#)
           NIL))
      (2
       . #(#.(BABEL:OCTETS-TO-STRING
                (MAKE-ARRAY '(3)
                  :ELEMENT-TYPE #2#
                  :INITIAL-CONTENTS '(228 186 140))
               . #3#)
           #.(BABEL:OCTETS-TO-STRING
                (MAKE-ARRAY '(3)
                  :ELEMENT-TYPE #2#
                  :INITIAL-CONTENTS '(229 188 144))
               . #3#)
           #.(BABEL:OCTETS-TO-STRING
                (MAKE-ARRAY '(3)
                  :ELEMENT-TYPE #2#
                  :INITIAL-CONTENTS '(232 178 179))
               . #3#)
           NIL))
      (3
       . #(#.(BABEL:OCTETS-TO-STRING
                (MAKE-ARRAY '(3)
                  :ELEMENT-TYPE #2#
                  :INITIAL-CONTENTS '(228 184 137))
               . #3#)
           #.(BABEL:OCTETS-TO-STRING
                (MAKE-ARRAY '(3)
                  :ELEMENT-TYPE #2#
                  :INITIAL-CONTENTS '(229 143 130))
               . #3#)
           #.(BABEL:OCTETS-TO-STRING
                (MAKE-ARRAY '(3)
                  :ELEMENT-TYPE #2#
                  :INITIAL-CONTENTS '(229 143 131))
               . #3#)
           NIL))
      (4
       . #(#4=#.(BABEL:OCTETS-TO-STRING
                   (MAKE-ARRAY '(3)
                     :ELEMENT-TYPE #2#
                     :INITIAL-CONTENTS '(229 155 155))
                  . #3#)
           #4#
           #.(BABEL:OCTETS-TO-STRING
                (MAKE-ARRAY '(3)
                  :ELEMENT-TYPE #2#
                  :INITIAL-CONTENTS '(232 130 134))
               . #3#)
           NIL))
      (5
       . #(#5=#.(BABEL:OCTETS-TO-STRING
                   (MAKE-ARRAY '(3)
                     :ELEMENT-TYPE #2#
                     :INITIAL-CONTENTS '(228 186 148))
                  . #3#)
           #5#
           #.(BABEL:OCTETS-TO-STRING
                (MAKE-ARRAY '(3)
                  :ELEMENT-TYPE #2#
                  :INITIAL-CONTENTS '(228 188 141))
               . #3#)
           NIL))
      (6
       . #(#6=#.(BABEL:OCTETS-TO-STRING
                   (MAKE-ARRAY '(3)
                     :ELEMENT-TYPE #2#
                     :INITIAL-CONTENTS '(229 133 173))
                  . #3#)
           #6#
           #.(BABEL:OCTETS-TO-STRING
                (MAKE-ARRAY '(3)
                  :ELEMENT-TYPE #2#
                  :INITIAL-CONTENTS '(233 153 184))
               . #3#)
           NIL))
      (7
       . #(#7=#.(BABEL:OCTETS-TO-STRING
                   (MAKE-ARRAY '(3)
                     :ELEMENT-TYPE #2#
                     :INITIAL-CONTENTS '(228 184 131))
                  . #3#)
           #7#
           #.(BABEL:OCTETS-TO-STRING
                (MAKE-ARRAY '(3)
                  :ELEMENT-TYPE #2#
                  :INITIAL-CONTENTS '(230 159 146))
               . #3#)
           NIL))
      (8
       . #(#8=#.(BABEL:OCTETS-TO-STRING
                   (MAKE-ARRAY '(3)
                     :ELEMENT-TYPE #2#
                     :INITIAL-CONTENTS '(229 133 171))
                  . #3#)
           #8#
           #.(BABEL:OCTETS-TO-STRING
                (MAKE-ARRAY '(3)
                  :ELEMENT-TYPE #2#
                  :INITIAL-CONTENTS '(230 141 140))
               . #3#)
           NIL))
      (9
       . #(#9=#.(BABEL:OCTETS-TO-STRING
                   (MAKE-ARRAY '(3)
                     :ELEMENT-TYPE #2#
                     :INITIAL-CONTENTS '(228 185 157))
                  . #3#)
           #9#
           #.(BABEL:OCTETS-TO-STRING
                (MAKE-ARRAY '(3)
                  :ELEMENT-TYPE #2#
                  :INITIAL-CONTENTS '(231 142 150))
               . #3#)
           NIL))))
  "A vector of (<normal-octets> <financial-octets> <old-octets> <alternative-in-BMP-of-Unicode>)")

(DEFCONSTANT +JP-NUMERAL-POWER-ALIST+
    (QUOTE
     ((0
       . #(#1=#.(BABEL:OCTETS-TO-STRING
                   (MAKE-ARRAY '(0)
                     :ELEMENT-TYPE #2='(UNSIGNED-BYTE 8)
                     :INITIAL-CONTENTS 'NIL)
                  . #3=(:ENCODING :UTF-8))
           #1# #1# NIL))
      (1
       . #(#.(BABEL:OCTETS-TO-STRING
                (MAKE-ARRAY '(3)
                  :ELEMENT-TYPE #2#
                  :INITIAL-CONTENTS '(229 141 129))
               . #3#)
           #4=#.(BABEL:OCTETS-TO-STRING
                   (MAKE-ARRAY '(3)
                     :ELEMENT-TYPE #2#
                     :INITIAL-CONTENTS '(230 139 190))
                  . #3#)
           #4# NIL))
      (2
       . #(#5=#.(BABEL:OCTETS-TO-STRING
                   (MAKE-ARRAY '(3)
                     :ELEMENT-TYPE #2#
                     :INITIAL-CONTENTS '(231 153 190))
                  . #3#)
           #5#
           #.(BABEL:OCTETS-TO-STRING
                (MAKE-ARRAY '(3)
                  :ELEMENT-TYPE #2#
                  :INITIAL-CONTENTS '(228 189 176))
               . #3#)
           NIL))
      (3
       . #(#6=#.(BABEL:OCTETS-TO-STRING
                   (MAKE-ARRAY '(3)
                     :ELEMENT-TYPE #2#
                     :INITIAL-CONTENTS '(229 141 131))
                  . #3#)
           #6#
           #.(BABEL:OCTETS-TO-STRING
                (MAKE-ARRAY '(3)
                  :ELEMENT-TYPE #2#
                  :INITIAL-CONTENTS '(228 187 159))
               . #3#)
           NIL))
      (4
       . #(#7=#.(BABEL:OCTETS-TO-STRING
                   (MAKE-ARRAY '(3)
                     :ELEMENT-TYPE #2#
                     :INITIAL-CONTENTS '(228 184 135))
                  . #3#)
           #7#
           #.(BABEL:OCTETS-TO-STRING
                (MAKE-ARRAY '(3)
                  :ELEMENT-TYPE #2#
                  :INITIAL-CONTENTS '(232 144 172))
               . #3#)
           NIL))
      (8
       . #(#8=#.(BABEL:OCTETS-TO-STRING
                   (MAKE-ARRAY '(3)
                     :ELEMENT-TYPE #2#
                     :INITIAL-CONTENTS '(229 132 132))
                  . #3#)
           #8# #8# NIL))
      (12
       . #(#9=#.(BABEL:OCTETS-TO-STRING
                   (MAKE-ARRAY '(3)
                     :ELEMENT-TYPE #2#
                     :INITIAL-CONTENTS '(229 133 134))
                  . #3#)
           #9# #9# NIL))
      (16
       . #(#10=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(3)
                      :ELEMENT-TYPE #2#
                      :INITIAL-CONTENTS '(228 186 172))
                   . #3#)
           #10# #10# NIL))
      (20
       . #(#11=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(3)
                      :ELEMENT-TYPE #2#
                      :INITIAL-CONTENTS '(229 158 147))
                   . #3#)
           #11# #11# NIL))
      (24
       . #(#12=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(4)
                      :ELEMENT-TYPE #2#
                      :INITIAL-CONTENTS '(240 165 157 177))
                   . #3#)
           #12#
           #.(BABEL:OCTETS-TO-STRING
                (MAKE-ARRAY '(3)
                  :ELEMENT-TYPE #2#
                  :INITIAL-CONTENTS '(231 167 173))
               . #3#)
           #.(BABEL:OCTETS-TO-STRING
                (MAKE-ARRAY '(3)
                  :ELEMENT-TYPE #2#
                  :INITIAL-CONTENTS '(231 167 173))
               . #3#)))
      (28
       . #(#13=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(3)
                      :ELEMENT-TYPE #2#
                      :INITIAL-CONTENTS '(231 169 163))
                   . #3#)
           #13# #13# NIL))
      (32
       . #(#14=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(3)
                      :ELEMENT-TYPE #2#
                      :INITIAL-CONTENTS '(230 186 157))
                   . #3#)
           #14# #14# NIL))
      (36
       . #(#15=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(3)
                      :ELEMENT-TYPE #2#
                      :INITIAL-CONTENTS '(230 190 151))
                   . #3#)
           #15# #15# NIL))
      (40
       . #(#16=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(3)
                      :ELEMENT-TYPE #2#
                      :INITIAL-CONTENTS '(230 173 163))
                   . #3#)
           #16# #16# NIL))
      (44
       . #(#17=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(3)
                      :ELEMENT-TYPE #2#
                      :INITIAL-CONTENTS '(232 188 137))
                   . #3#)
           #17# #17# NIL))
      (48
       . #(#18=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(3)
                      :ELEMENT-TYPE #2#
                      :INITIAL-CONTENTS '(230 165 181))
                   . #3#)
           #18# #18# NIL))
      (52
       . #(#19=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(9)
                      :ELEMENT-TYPE #2#
                      :INITIAL-CONTENTS '(230
                                          129
                                          146
                                          230
                                          178
                                          179
                                          230
                                          178
                                          153))
                   . #3#)
           #19# #19# NIL))
      (56
       . #(#20=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(9)
                      :ELEMENT-TYPE #2#
                      :INITIAL-CONTENTS '(233
                                          152
                                          191
                                          229
                                          131
                                          167
                                          231
                                          165
                                          135))
                   . #3#)
           #20# #20# NIL))
      (60
       . #(#21=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(9)
                      :ELEMENT-TYPE #2#
                      :INITIAL-CONTENTS '(233
                                          130
                                          163
                                          231
                                          148
                                          177
                                          228
                                          187
                                          150))
                   . #3#)
           #21# #21# NIL))
      (64
       . #(#22=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(12)
                      :ELEMENT-TYPE #2#
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
                                          176))
                   . #3#)
           #22# #22# NIL))
      (68
       . #(#23=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(12)
                      :ELEMENT-TYPE #2#
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
                                          176))
                   . #3#)
           #23# #23# NIL))
      (-1
       . #(#24=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(3)
                      :ELEMENT-TYPE #2#
                      :INITIAL-CONTENTS '(229 136 134))
                   . #3#)
           #24# #24# NIL))
      (-2
       . #(#25=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(3)
                      :ELEMENT-TYPE #2#
                      :INITIAL-CONTENTS '(229 142 152))
                   . #3#)
           #25#
           #.(BABEL:OCTETS-TO-STRING
                (MAKE-ARRAY '(3)
                  :ELEMENT-TYPE #2#
                  :INITIAL-CONTENTS '(233 135 144))
               . #3#)
           NIL))
      (-3
       . #(#26=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(3)
                      :ELEMENT-TYPE #2#
                      :INITIAL-CONTENTS '(230 175 155))
                   . #3#)
           #26#
           #.(BABEL:OCTETS-TO-STRING
                (MAKE-ARRAY '(3)
                  :ELEMENT-TYPE #2#
                  :INITIAL-CONTENTS '(230 175 171))
               . #3#)
           NIL))
      (-4
       . #(#27=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(3)
                      :ELEMENT-TYPE #2#
                      :INITIAL-CONTENTS '(231 179 184))
                   . #3#)
           #27#
           #.(BABEL:OCTETS-TO-STRING
                (MAKE-ARRAY '(3)
                  :ELEMENT-TYPE #2#
                  :INITIAL-CONTENTS '(231 181 178))
               . #3#)
           NIL))
      (-5
       . #(#28=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(3)
                      :ELEMENT-TYPE #2#
                      :INITIAL-CONTENTS '(229 191 189))
                   . #3#)
           #28# #28# NIL))
      (-6
       . #(#29=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(3)
                      :ELEMENT-TYPE #2#
                      :INITIAL-CONTENTS '(229 190 174))
                   . #3#)
           #29# #29# NIL))
      (-7
       . #(#30=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(3)
                      :ELEMENT-TYPE #2#
                      :INITIAL-CONTENTS '(231 185 138))
                   . #3#)
           #30# #30# NIL))
      (-8
       . #(#31=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(3)
                      :ELEMENT-TYPE #2#
                      :INITIAL-CONTENTS '(230 178 153))
                   . #3#)
           #31# #31# NIL))
      (-9
       . #(#32=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(3)
                      :ELEMENT-TYPE #2#
                      :INITIAL-CONTENTS '(229 161 181))
                   . #3#)
           #32# #32# NIL))
      (-10
       . #(#33=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(3)
                      :ELEMENT-TYPE #2#
                      :INITIAL-CONTENTS '(229 159 131))
                   . #3#)
           #33# #33# NIL))
      (-11
       . #(#34=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(3)
                      :ELEMENT-TYPE #2#
                      :INITIAL-CONTENTS '(230 184 186))
                   . #3#)
           #34# #34# NIL))
      (-12
       . #(#35=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(3)
                      :ELEMENT-TYPE #2#
                      :INITIAL-CONTENTS '(230 188 160))
                   . #3#)
           #35# #35# NIL))
      (-13
       . #(#36=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(6)
                      :ELEMENT-TYPE #2#
                      :INITIAL-CONTENTS '(230 168 161 231 179 138))
                   . #3#)
           #36# #36# NIL))
      (-14
       . #(#37=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(6)
                      :ELEMENT-TYPE #2#
                      :INITIAL-CONTENTS '(233 128 161 229 183 161))
                   . #3#)
           #37# #37# NIL))
      (-15
       . #(#38=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(6)
                      :ELEMENT-TYPE #2#
                      :INITIAL-CONTENTS '(233 160 136 232 135 190))
                   . #3#)
           #38# #38# NIL))
      (-16
       . #(#39=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(6)
                      :ELEMENT-TYPE #2#
                      :INITIAL-CONTENTS '(231 158 172 230 129 175))
                   . #3#)
           #39# #39# NIL))
      (-17
       . #(#40=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(6)
                      :ELEMENT-TYPE #2#
                      :INITIAL-CONTENTS '(229 188 190 230 140 135))
                   . #3#)
           #40# #40# NIL))
      (-18
       . #(#41=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(6)
                      :ELEMENT-TYPE #2#
                      :INITIAL-CONTENTS '(229 136 185 233 130 163))
                   . #3#)
           #41# #41# NIL))
      (-19
       . #(#42=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(6)
                      :ELEMENT-TYPE #2#
                      :INITIAL-CONTENTS '(229 133 173 229 190 179))
                   . #3#)
           #42# #42# NIL))
      (-20
       . #(#43=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(6)
                      :ELEMENT-TYPE #2#
                      :INITIAL-CONTENTS '(232 153 154 231 169 186))
                   . #3#)
           #43# #43# NIL))
      (-21
       . #(#44=#.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(6)
                      :ELEMENT-TYPE #2#
                      :INITIAL-CONTENTS '(230 184 133 230 181 132))
                   . #3#)
           #44# #44# NIL))))
  "An alist of (<power> . (<normal-octets> <financial-octets> <old-octets> <alternative-in-BMP-of-Unicode>))")
