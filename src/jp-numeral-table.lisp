(IN-PACKAGE "JP-NUMERAL")

(DEFINE-CONSTANT +TABLE-NORMAL-INDEX+ 0 :TEST 'EQUALP :DOCUMENTATION
                 NIL)
(DEFINE-CONSTANT +TABLE-FORMAL-INDEX+ 1 :TEST 'EQUALP :DOCUMENTATION
                 NIL)
(DEFINE-CONSTANT +TABLE-OLD-INDEX+ 2 :TEST 'EQUALP :DOCUMENTATION NIL)
(DEFINE-CONSTANT +TABLE-POSITIONAL-INDEX+ 3 :TEST 'EQUALP
                 :DOCUMENTATION NIL)

(DEFINE-CONSTANT +DIGITS+
                 '#(#(#1=#.(BABEL:OCTETS-TO-STRING
                              (MAKE-ARRAY '(3)
                                :INITIAL-CONTENTS '(227 128 135)
                                . #2=(:ELEMENT-TYPE
                                      '(UNSIGNED-BYTE 8)))
                             . #3=(:ENCODING :UTF-8))
                      #1#
                      #.(BABEL:OCTETS-TO-STRING
                           (MAKE-ARRAY '(3)
                             :INITIAL-CONTENTS '(233 155 182)
                             . #2#)
                          . #3#)
                      #1#)
                    #(#4=#.(BABEL:OCTETS-TO-STRING
                              (MAKE-ARRAY '(3)
                                :INITIAL-CONTENTS '(228 184 128)
                                . #2#)
                             . #3#)
                      #.(BABEL:OCTETS-TO-STRING
                           (MAKE-ARRAY '(3)
                             :INITIAL-CONTENTS '(229 163 177)
                             . #2#)
                          . #3#)
                      #.(BABEL:OCTETS-TO-STRING
                           (MAKE-ARRAY '(3)
                             :INITIAL-CONTENTS '(229 163 185)
                             . #2#)
                          . #3#)
                      #4#)
                    #(#5=#.(BABEL:OCTETS-TO-STRING
                              (MAKE-ARRAY '(3)
                                :INITIAL-CONTENTS '(228 186 140)
                                . #2#)
                             . #3#)
                      #.(BABEL:OCTETS-TO-STRING
                           (MAKE-ARRAY '(3)
                             :INITIAL-CONTENTS '(229 188 144)
                             . #2#)
                          . #3#)
                      #.(BABEL:OCTETS-TO-STRING
                           (MAKE-ARRAY '(3)
                             :INITIAL-CONTENTS '(232 178 179)
                             . #2#)
                          . #3#)
                      #5#)
                    #(#6=#.(BABEL:OCTETS-TO-STRING
                              (MAKE-ARRAY '(3)
                                :INITIAL-CONTENTS '(228 184 137)
                                . #2#)
                             . #3#)
                      #.(BABEL:OCTETS-TO-STRING
                           (MAKE-ARRAY '(3)
                             :INITIAL-CONTENTS '(229 143 130)
                             . #2#)
                          . #3#)
                      #.(BABEL:OCTETS-TO-STRING
                           (MAKE-ARRAY '(3)
                             :INITIAL-CONTENTS '(229 143 131)
                             . #2#)
                          . #3#)
                      #6#)
                    #(#7=#.(BABEL:OCTETS-TO-STRING
                              (MAKE-ARRAY '(3)
                                :INITIAL-CONTENTS '(229 155 155)
                                . #2#)
                             . #3#)
                      #7#
                      #.(BABEL:OCTETS-TO-STRING
                           (MAKE-ARRAY '(3)
                             :INITIAL-CONTENTS '(232 130 134)
                             . #2#)
                          . #3#)
                      #7#)
                    #(#8=#.(BABEL:OCTETS-TO-STRING
                              (MAKE-ARRAY '(3)
                                :INITIAL-CONTENTS '(228 186 148)
                                . #2#)
                             . #3#)
                      #8#
                      #.(BABEL:OCTETS-TO-STRING
                           (MAKE-ARRAY '(3)
                             :INITIAL-CONTENTS '(228 188 141)
                             . #2#)
                          . #3#)
                      #8#)
                    #(#9=#.(BABEL:OCTETS-TO-STRING
                              (MAKE-ARRAY '(3)
                                :INITIAL-CONTENTS '(229 133 173)
                                . #2#)
                             . #3#)
                      #9#
                      #.(BABEL:OCTETS-TO-STRING
                           (MAKE-ARRAY '(3)
                             :INITIAL-CONTENTS '(233 153 184)
                             . #2#)
                          . #3#)
                      #9#)
                    #(#10=#.(BABEL:OCTETS-TO-STRING
                               (MAKE-ARRAY
                                '(3)
                                :INITIAL-CONTENTS
                                '(228 184 131)
                                . #2#)
                              . #3#)
                      #10#
                      #.(BABEL:OCTETS-TO-STRING
                           (MAKE-ARRAY '(3)
                             :INITIAL-CONTENTS '(230 159 146)
                             . #2#)
                          . #3#)
                      #10#)
                    #(#11=#.(BABEL:OCTETS-TO-STRING
                               (MAKE-ARRAY
                                '(3)
                                :INITIAL-CONTENTS
                                '(229 133 171)
                                . #2#)
                              . #3#)
                      #11#
                      #.(BABEL:OCTETS-TO-STRING
                           (MAKE-ARRAY '(3)
                             :INITIAL-CONTENTS '(230 141 140)
                             . #2#)
                          . #3#)
                      #11#)
                    #(#12=#.(BABEL:OCTETS-TO-STRING
                               (MAKE-ARRAY
                                '(3)
                                :INITIAL-CONTENTS
                                '(228 185 157)
                                . #2#)
                              . #3#)
                      #12#
                      #.(BABEL:OCTETS-TO-STRING
                           (MAKE-ARRAY '(3)
                             :INITIAL-CONTENTS '(231 142 150)
                             . #2#)
                          . #3#)
                      #12#))
                 :TEST 'EQUALP :DOCUMENTATION
                 "A vector of (<normal> <formal> <old> <positional>)")

(DEFINE-CONSTANT +POWER-ALIST+
                 '((0
                    . #(#1=#.(BABEL:OCTETS-TO-STRING
                                (MAKE-ARRAY
                                 '(0)
                                 :INITIAL-CONTENTS
                                 'NIL
                                 . #2=(:ELEMENT-TYPE
                                       '(UNSIGNED-BYTE 8)))
                               . #3=(:ENCODING :UTF-8))
                        #1# #1# #1#))
                   (1
                    . #(#5=#.(BABEL:OCTETS-TO-STRING
                                (MAKE-ARRAY
                                 '(3)
                                 :INITIAL-CONTENTS
                                 '(229 141 129)
                                 . #2#)
                               . #3#)
                        #4=#.(BABEL:OCTETS-TO-STRING
                                (MAKE-ARRAY
                                 '(3)
                                 :INITIAL-CONTENTS
                                 '(230 139 190)
                                 . #2#)
                               . #3#)
                        #4# #5#))
                   (2
                    . #(#6=#.(BABEL:OCTETS-TO-STRING
                                (MAKE-ARRAY
                                 '(3)
                                 :INITIAL-CONTENTS
                                 '(231 153 190)
                                 . #2#)
                               . #3#)
                        #6#
                        #.(BABEL:OCTETS-TO-STRING
                             (MAKE-ARRAY '(3)
                               :INITIAL-CONTENTS '(228 189 176)
                               . #2#)
                            . #3#)
                        #6#))
                   (3
                    . #(#7=#.(BABEL:OCTETS-TO-STRING
                                (MAKE-ARRAY
                                 '(3)
                                 :INITIAL-CONTENTS
                                 '(229 141 131)
                                 . #2#)
                               . #3#)
                        #7#
                        #.(BABEL:OCTETS-TO-STRING
                             (MAKE-ARRAY '(3)
                               :INITIAL-CONTENTS '(228 187 159)
                               . #2#)
                            . #3#)
                        #7#))
                   (4
                    . #(#8=#.(BABEL:OCTETS-TO-STRING
                                (MAKE-ARRAY
                                 '(3)
                                 :INITIAL-CONTENTS
                                 '(228 184 135)
                                 . #2#)
                               . #3#)
                        #8#
                        #.(BABEL:OCTETS-TO-STRING
                             (MAKE-ARRAY '(3)
                               :INITIAL-CONTENTS '(232 144 172)
                               . #2#)
                            . #3#)
                        #8#))
                   (8
                    . #(#9=#.(BABEL:OCTETS-TO-STRING
                                (MAKE-ARRAY
                                 '(3)
                                 :INITIAL-CONTENTS
                                 '(229 132 132)
                                 . #2#)
                               . #3#)
                        #9# #9# #9#))
                   (12
                    . #(#10=#.(BABEL:OCTETS-TO-STRING
                                 (MAKE-ARRAY
                                  '(3)
                                  :INITIAL-CONTENTS
                                  '(229 133 134)
                                  . #2#)
                                . #3#)
                        #10# #10# #10#))
                   (16
                    . #(#11=#.(BABEL:OCTETS-TO-STRING
                                 (MAKE-ARRAY
                                  '(3)
                                  :INITIAL-CONTENTS
                                  '(228 186 172)
                                  . #2#)
                                . #3#)
                        #11# #11# #11#))
                   (20
                    . #(#12=#.(BABEL:OCTETS-TO-STRING
                                 (MAKE-ARRAY
                                  '(3)
                                  :INITIAL-CONTENTS
                                  '(229 158 147)
                                  . #2#)
                                . #3#)
                        #12# #12# #12#))
                   (24
                    . #(#13=#.(BABEL:OCTETS-TO-STRING
                                 (MAKE-ARRAY
                                  '(4)
                                  :INITIAL-CONTENTS
                                  '(240 165 157 177)
                                  . #2#)
                                . #3#)
                        #13#
                        #.(BABEL:OCTETS-TO-STRING
                             (MAKE-ARRAY '(3)
                               :INITIAL-CONTENTS '(231 167 173)
                               . #2#)
                            . #3#)
                        #13#))
                   (28
                    . #(#14=#.(BABEL:OCTETS-TO-STRING
                                 (MAKE-ARRAY
                                  '(3)
                                  :INITIAL-CONTENTS
                                  '(231 169 163)
                                  . #2#)
                                . #3#)
                        #14# #14# #14#))
                   (32
                    . #(#15=#.(BABEL:OCTETS-TO-STRING
                                 (MAKE-ARRAY
                                  '(3)
                                  :INITIAL-CONTENTS
                                  '(230 186 157)
                                  . #2#)
                                . #3#)
                        #15# #15# #15#))
                   (36
                    . #(#16=#.(BABEL:OCTETS-TO-STRING
                                 (MAKE-ARRAY
                                  '(3)
                                  :INITIAL-CONTENTS
                                  '(230 190 151)
                                  . #2#)
                                . #3#)
                        #16# #16# #16#))
                   (40
                    . #(#17=#.(BABEL:OCTETS-TO-STRING
                                 (MAKE-ARRAY
                                  '(3)
                                  :INITIAL-CONTENTS
                                  '(230 173 163)
                                  . #2#)
                                . #3#)
                        #17# #17# #17#))
                   (44
                    . #(#18=#.(BABEL:OCTETS-TO-STRING
                                 (MAKE-ARRAY
                                  '(3)
                                  :INITIAL-CONTENTS
                                  '(232 188 137)
                                  . #2#)
                                . #3#)
                        #18# #18# #18#))
                   (48
                    . #(#19=#.(BABEL:OCTETS-TO-STRING
                                 (MAKE-ARRAY
                                  '(3)
                                  :INITIAL-CONTENTS
                                  '(230 165 181)
                                  . #2#)
                                . #3#)
                        #19# #19# #19#))
                   (52
                    . #(#20=#.(BABEL:OCTETS-TO-STRING
                                 (MAKE-ARRAY
                                  '(9)
                                  :INITIAL-CONTENTS
                                  '(230
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
                        #20# #20# #20#))
                   (56
                    . #(#21=#.(BABEL:OCTETS-TO-STRING
                                 (MAKE-ARRAY
                                  '(9)
                                  :INITIAL-CONTENTS
                                  '(233
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
                        #21# #21# #21#))
                   (60
                    . #(#22=#.(BABEL:OCTETS-TO-STRING
                                 (MAKE-ARRAY
                                  '(9)
                                  :INITIAL-CONTENTS
                                  '(233
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
                        #22# #22# #22#))
                   (64
                    . #(#23=#.(BABEL:OCTETS-TO-STRING
                                 (MAKE-ARRAY
                                  '(12)
                                  :INITIAL-CONTENTS
                                  '(228
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
                        #23# #23# #23#))
                   (68
                    . #(#24=#.(BABEL:OCTETS-TO-STRING
                                 (MAKE-ARRAY
                                  '(12)
                                  :INITIAL-CONTENTS
                                  '(231
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
                        #24# #24# #24#))
                   (-1
                    . #(#25=#.(BABEL:OCTETS-TO-STRING
                                 (MAKE-ARRAY
                                  '(3)
                                  :INITIAL-CONTENTS
                                  '(229 136 134)
                                  . #2#)
                                . #3#)
                        #25# #25# #25#))
                   (-2
                    . #(#26=#.(BABEL:OCTETS-TO-STRING
                                 (MAKE-ARRAY
                                  '(3)
                                  :INITIAL-CONTENTS
                                  '(229 142 152)
                                  . #2#)
                                . #3#)
                        #26#
                        #.(BABEL:OCTETS-TO-STRING
                             (MAKE-ARRAY '(3)
                               :INITIAL-CONTENTS '(233 135 144)
                               . #2#)
                            . #3#)
                        #26#))
                   (-3
                    . #(#27=#.(BABEL:OCTETS-TO-STRING
                                 (MAKE-ARRAY
                                  '(3)
                                  :INITIAL-CONTENTS
                                  '(230 175 155)
                                  . #2#)
                                . #3#)
                        #27#
                        #.(BABEL:OCTETS-TO-STRING
                             (MAKE-ARRAY '(3)
                               :INITIAL-CONTENTS '(230 175 171)
                               . #2#)
                            . #3#)
                        #27#))
                   (-4
                    . #(#28=#.(BABEL:OCTETS-TO-STRING
                                 (MAKE-ARRAY
                                  '(3)
                                  :INITIAL-CONTENTS
                                  '(231 179 184)
                                  . #2#)
                                . #3#)
                        #28#
                        #.(BABEL:OCTETS-TO-STRING
                             (MAKE-ARRAY '(3)
                               :INITIAL-CONTENTS '(231 181 178)
                               . #2#)
                            . #3#)
                        #28#))
                   (-5
                    . #(#29=#.(BABEL:OCTETS-TO-STRING
                                 (MAKE-ARRAY
                                  '(3)
                                  :INITIAL-CONTENTS
                                  '(229 191 189)
                                  . #2#)
                                . #3#)
                        #29# #29# #29#))
                   (-6
                    . #(#30=#.(BABEL:OCTETS-TO-STRING
                                 (MAKE-ARRAY
                                  '(3)
                                  :INITIAL-CONTENTS
                                  '(229 190 174)
                                  . #2#)
                                . #3#)
                        #30# #30# #30#))
                   (-7
                    . #(#31=#.(BABEL:OCTETS-TO-STRING
                                 (MAKE-ARRAY
                                  '(3)
                                  :INITIAL-CONTENTS
                                  '(231 185 138)
                                  . #2#)
                                . #3#)
                        #31# #31# #31#))
                   (-8
                    . #(#32=#.(BABEL:OCTETS-TO-STRING
                                 (MAKE-ARRAY
                                  '(3)
                                  :INITIAL-CONTENTS
                                  '(230 178 153)
                                  . #2#)
                                . #3#)
                        #32# #32# #32#))
                   (-9
                    . #(#33=#.(BABEL:OCTETS-TO-STRING
                                 (MAKE-ARRAY
                                  '(3)
                                  :INITIAL-CONTENTS
                                  '(229 161 181)
                                  . #2#)
                                . #3#)
                        #33# #33# #33#))
                   (-10
                    . #(#34=#.(BABEL:OCTETS-TO-STRING
                                 (MAKE-ARRAY
                                  '(3)
                                  :INITIAL-CONTENTS
                                  '(229 159 131)
                                  . #2#)
                                . #3#)
                        #34# #34# #34#))
                   (-11
                    . #(#35=#.(BABEL:OCTETS-TO-STRING
                                 (MAKE-ARRAY
                                  '(3)
                                  :INITIAL-CONTENTS
                                  '(230 184 186)
                                  . #2#)
                                . #3#)
                        #35# #35# #35#))
                   (-12
                    . #(#36=#.(BABEL:OCTETS-TO-STRING
                                 (MAKE-ARRAY
                                  '(3)
                                  :INITIAL-CONTENTS
                                  '(230 188 160)
                                  . #2#)
                                . #3#)
                        #36# #36# #36#))
                   (-13
                    . #(#37=#.(BABEL:OCTETS-TO-STRING
                                 (MAKE-ARRAY
                                  '(6)
                                  :INITIAL-CONTENTS
                                  '(230 168 161 231 179 138)
                                  . #2#)
                                . #3#)
                        #37# #37# #37#))
                   (-14
                    . #(#38=#.(BABEL:OCTETS-TO-STRING
                                 (MAKE-ARRAY
                                  '(6)
                                  :INITIAL-CONTENTS
                                  '(233 128 161 229 183 161)
                                  . #2#)
                                . #3#)
                        #38# #38# #38#))
                   (-15
                    . #(#39=#.(BABEL:OCTETS-TO-STRING
                                 (MAKE-ARRAY
                                  '(6)
                                  :INITIAL-CONTENTS
                                  '(233 160 136 232 135 190)
                                  . #2#)
                                . #3#)
                        #39# #39# #39#))
                   (-16
                    . #(#40=#.(BABEL:OCTETS-TO-STRING
                                 (MAKE-ARRAY
                                  '(6)
                                  :INITIAL-CONTENTS
                                  '(231 158 172 230 129 175)
                                  . #2#)
                                . #3#)
                        #40# #40# #40#))
                   (-17
                    . #(#41=#.(BABEL:OCTETS-TO-STRING
                                 (MAKE-ARRAY
                                  '(6)
                                  :INITIAL-CONTENTS
                                  '(229 188 190 230 140 135)
                                  . #2#)
                                . #3#)
                        #41# #41# #41#))
                   (-18
                    . #(#42=#.(BABEL:OCTETS-TO-STRING
                                 (MAKE-ARRAY
                                  '(6)
                                  :INITIAL-CONTENTS
                                  '(229 136 185 233 130 163)
                                  . #2#)
                                . #3#)
                        #42# #42# #42#))
                   (-19
                    . #(#43=#.(BABEL:OCTETS-TO-STRING
                                 (MAKE-ARRAY
                                  '(6)
                                  :INITIAL-CONTENTS
                                  '(229 133 173 229 190 179)
                                  . #2#)
                                . #3#)
                        #43# #43# #43#))
                   (-20
                    . #(#44=#.(BABEL:OCTETS-TO-STRING
                                 (MAKE-ARRAY
                                  '(6)
                                  :INITIAL-CONTENTS
                                  '(232 153 154 231 169 186)
                                  . #2#)
                                . #3#)
                        #44# #44# #44#))
                   (-21
                    . #(#45=#.(BABEL:OCTETS-TO-STRING
                                 (MAKE-ARRAY
                                  '(6)
                                  :INITIAL-CONTENTS
                                  '(230 184 133 230 181 132)
                                  . #2#)
                                . #3#)
                        #45# #45# #45#)))
                 :TEST 'EQUALP :DOCUMENTATION
                 "An alist of (<power> . (<normal> <formal> <old> <positional>))")

(DEFINE-CONSTANT +POWER-MAX+ 68 :TEST 'EQUALP :DOCUMENTATION NIL)

(DEFINE-CONSTANT +POWER-MIN+ -21 :TEST 'EQUALP :DOCUMENTATION NIL)

(DEFINE-CONSTANT +MINUS-SIGN+
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
                        (MAKE-ARRAY '(3)
                          :INITIAL-CONTENTS '(226 136 146)
                          . #1#)
                       . #2#))
                 :TEST 'EQUALP :DOCUMENTATION
                 "A vector of (<normal> <formal> <old> <positional>")

(DEFINE-CONSTANT +FRACTION-PARTS-OF+
                 #(#1=#.(BABEL:OCTETS-TO-STRING
                           (MAKE-ARRAY '(6)
                             :INITIAL-CONTENTS '(229
                                                 136
                                                 134
                                                 227
                                                 129
                                                 174)
                             . #2=(:ELEMENT-TYPE '(UNSIGNED-BYTE 8)))
                          . #3=(:ENCODING :UTF-8))
                   #1#
                   #.(BABEL:OCTETS-TO-STRING
                        (MAKE-ARRAY '(6)
                          :INITIAL-CONTENTS '(229 136 134 228 185 139)
                          . #2#)
                       . #3#)
                   #.(BABEL:OCTETS-TO-STRING
                        (MAKE-ARRAY '(3)
                          :INITIAL-CONTENTS '(239 188 143)
                          . #2#)
                       . #3#))
                 :TEST 'EQUALP :DOCUMENTATION
                 "A vector of (<normal> <formal> <old> <positional>")

(DEFINE-CONSTANT +RADIX-POINT+
                 #(#1=#.(BABEL:OCTETS-TO-STRING
                           (MAKE-ARRAY '(0)
                             :INITIAL-CONTENTS 'NIL
                             . #2=(:ELEMENT-TYPE '(UNSIGNED-BYTE 8)))
                          . #3=(:ENCODING :UTF-8))
                   #1# #1#
                   #.(BABEL:OCTETS-TO-STRING
                        (MAKE-ARRAY '(3)
                          :INITIAL-CONTENTS '(227 131 187)
                          . #2#)
                       . #3#))
                 :TEST 'EQUALP :DOCUMENTATION
                 "A vector of (<normal> <formal> <old> <positional>")

(DEFINE-CONSTANT +YEN+
                 #(#1=#.(BABEL:OCTETS-TO-STRING
                           (MAKE-ARRAY '(3)
                             :INITIAL-CONTENTS '(229 134 134)
                             . #2=(:ELEMENT-TYPE '(UNSIGNED-BYTE 8)))
                          . #3=(:ENCODING :UTF-8))
                   #1#
                   #.(BABEL:OCTETS-TO-STRING
                        (MAKE-ARRAY '(3)
                          :INITIAL-CONTENTS '(229 156 147)
                          . #2#)
                       . #3#)
                   #1#)
                 :TEST 'EQUALP :DOCUMENTATION
                 "A vector of (<normal> <formal> <old> <positional>")

(DEFINE-CONSTANT +SEN+
                 #(#1=#.(BABEL:OCTETS-TO-STRING
                           (MAKE-ARRAY '(3)
                             :INITIAL-CONTENTS '(233 138 173)
                             . #2=(:ELEMENT-TYPE '(UNSIGNED-BYTE 8)))
                          . #3=(:ENCODING :UTF-8))
                   #1#
                   #.(BABEL:OCTETS-TO-STRING
                        (MAKE-ARRAY '(3)
                          :INITIAL-CONTENTS '(233 140 162)
                          . #2#)
                       . #3#)
                   #1#)
                 :TEST 'EQUALP :DOCUMENTATION
                 "A vector of (<normal> <formal> <old> <positional>")

(DEFINE-CONSTANT +WARI+
                 #(#1=#.(BABEL:OCTETS-TO-STRING
                           (MAKE-ARRAY '(3)
                             :INITIAL-CONTENTS '(229 137 178)
                             :ELEMENT-TYPE '(UNSIGNED-BYTE 8))
                          :ENCODING :UTF-8)
                   #1# #1# #1#)
                 :TEST 'EQUALP :DOCUMENTATION
                 "A vector of (<normal> <formal> <old> <positional>")
