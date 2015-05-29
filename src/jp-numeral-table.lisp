(IN-PACKAGE "JP-NUMERAL")

(DEFINE-CONSTANT +TABLE-NORMAL-INDEX+ 0 :TEST 'EQUALP :DOCUMENTATION NIL)
(DEFINE-CONSTANT +TABLE-FORMAL-INDEX+ 1 :TEST 'EQUALP :DOCUMENTATION NIL)
(DEFINE-CONSTANT +TABLE-OLD-INDEX+ 2 :TEST 'EQUALP :DOCUMENTATION NIL)
(DEFINE-CONSTANT +TABLE-POSITIONAL-INDEX+ 3 :TEST 'EQUALP :DOCUMENTATION NIL)

(DEFINE-CONSTANT +DIGITS+
                 '#(#(#1=#.(BABEL:OCTETS-TO-STRING
                            (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(227 128 135)
                                        . #2=(:ELEMENT-TYPE
                                              '(UNSIGNED-BYTE 8)))
                            . #3=(:ENCODING :UTF-8))
                      #1#
                      #.(BABEL:OCTETS-TO-STRING
                         (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(233 155 182)
                                     . #2#)
                         . #3#))
                    #(#.(BABEL:OCTETS-TO-STRING
                         (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(228 184 128)
                                     . #2#)
                         . #3#)
                      #.(BABEL:OCTETS-TO-STRING
                         (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(229 163 177)
                                     . #2#)
                         . #3#)
                      #.(BABEL:OCTETS-TO-STRING
                         (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(229 163 185)
                                     . #2#)
                         . #3#))
                    #(#.(BABEL:OCTETS-TO-STRING
                         (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(228 186 140)
                                     . #2#)
                         . #3#)
                      #.(BABEL:OCTETS-TO-STRING
                         (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(229 188 144)
                                     . #2#)
                         . #3#)
                      #.(BABEL:OCTETS-TO-STRING
                         (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(232 178 179)
                                     . #2#)
                         . #3#))
                    #(#.(BABEL:OCTETS-TO-STRING
                         (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(228 184 137)
                                     . #2#)
                         . #3#)
                      #.(BABEL:OCTETS-TO-STRING
                         (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(229 143 130)
                                     . #2#)
                         . #3#)
                      #.(BABEL:OCTETS-TO-STRING
                         (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(229 143 131)
                                     . #2#)
                         . #3#))
                    #(#4=#.(BABEL:OCTETS-TO-STRING
                            (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(229 155 155)
                                        . #2#)
                            . #3#)
                      #4#
                      #.(BABEL:OCTETS-TO-STRING
                         (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(232 130 134)
                                     . #2#)
                         . #3#))
                    #(#5=#.(BABEL:OCTETS-TO-STRING
                            (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(228 186 148)
                                        . #2#)
                            . #3#)
                      #5#
                      #.(BABEL:OCTETS-TO-STRING
                         (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(228 188 141)
                                     . #2#)
                         . #3#))
                    #(#6=#.(BABEL:OCTETS-TO-STRING
                            (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(229 133 173)
                                        . #2#)
                            . #3#)
                      #6#
                      #.(BABEL:OCTETS-TO-STRING
                         (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(233 153 184)
                                     . #2#)
                         . #3#))
                    #(#7=#.(BABEL:OCTETS-TO-STRING
                            (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(228 184 131)
                                        . #2#)
                            . #3#)
                      #7#
                      #.(BABEL:OCTETS-TO-STRING
                         (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(230 159 146)
                                     . #2#)
                         . #3#))
                    #(#8=#.(BABEL:OCTETS-TO-STRING
                            (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(229 133 171)
                                        . #2#)
                            . #3#)
                      #8#
                      #.(BABEL:OCTETS-TO-STRING
                         (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(230 141 140)
                                     . #2#)
                         . #3#))
                    #(#9=#.(BABEL:OCTETS-TO-STRING
                            (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(228 185 157)
                                        . #2#)
                            . #3#)
                      #9#
                      #.(BABEL:OCTETS-TO-STRING
                         (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(231 142 150)
                                     . #2#)
                         . #3#)))
                 :TEST 'EQUALP :DOCUMENTATION
                 "A vector of (<normal> <formal> <old>)")

(DEFINE-CONSTANT +POWER-ALIST+
                 '((1
                    . #(#.(BABEL:OCTETS-TO-STRING
                           (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(229 141 129)
                                       . #1=(:ELEMENT-TYPE '(UNSIGNED-BYTE 8)))
                           . #2=(:ENCODING :UTF-8))
                        #3=#.(BABEL:OCTETS-TO-STRING
                              (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(230 139 190)
                                          . #1#)
                              . #2#)
                        #3#))
                   (2
                    . #(#4=#.(BABEL:OCTETS-TO-STRING
                              (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(231 153 190)
                                          . #1#)
                              . #2#)
                        #4#
                        #.(BABEL:OCTETS-TO-STRING
                           (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(228 189 176)
                                       . #1#)
                           . #2#)))
                   (3
                    . #(#5=#.(BABEL:OCTETS-TO-STRING
                              (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(229 141 131)
                                          . #1#)
                              . #2#)
                        #5#
                        #.(BABEL:OCTETS-TO-STRING
                           (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(228 187 159)
                                       . #1#)
                           . #2#)))
                   (4
                    . #(#6=#.(BABEL:OCTETS-TO-STRING
                              (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(228 184 135)
                                          . #1#)
                              . #2#)
                        #6#
                        #.(BABEL:OCTETS-TO-STRING
                           (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(232 144 172)
                                       . #1#)
                           . #2#)))
                   (8
                    . #(#7=#.(BABEL:OCTETS-TO-STRING
                              (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(229 132 132)
                                          . #1#)
                              . #2#)
                        #7# #7#))
                   (12
                    . #(#8=#.(BABEL:OCTETS-TO-STRING
                              (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(229 133 134)
                                          . #1#)
                              . #2#)
                        #8# #8#))
                   (16
                    . #(#9=#.(BABEL:OCTETS-TO-STRING
                              (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(228 186 172)
                                          . #1#)
                              . #2#)
                        #9# #9#))
                   (20
                    . #(#10=#.(BABEL:OCTETS-TO-STRING
                               (MAKE-ARRAY '(3) :INITIAL-CONTENTS
                                           '(229 158 147) . #1#)
                               . #2#)
                        #10# #10#))
                   (24
                    . #(#11=#.(BABEL:OCTETS-TO-STRING
                               (MAKE-ARRAY '(4) :INITIAL-CONTENTS
                                           '(240 165 157 177) . #1#)
                               . #2#)
                        #11#
                        #.(BABEL:OCTETS-TO-STRING
                           (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(231 167 173)
                                       . #1#)
                           . #2#)))
                   (28
                    . #(#12=#.(BABEL:OCTETS-TO-STRING
                               (MAKE-ARRAY '(3) :INITIAL-CONTENTS
                                           '(231 169 163) . #1#)
                               . #2#)
                        #12# #12#))
                   (32
                    . #(#13=#.(BABEL:OCTETS-TO-STRING
                               (MAKE-ARRAY '(3) :INITIAL-CONTENTS
                                           '(230 186 157) . #1#)
                               . #2#)
                        #13# #13#))
                   (36
                    . #(#14=#.(BABEL:OCTETS-TO-STRING
                               (MAKE-ARRAY '(3) :INITIAL-CONTENTS
                                           '(230 190 151) . #1#)
                               . #2#)
                        #14# #14#))
                   (40
                    . #(#15=#.(BABEL:OCTETS-TO-STRING
                               (MAKE-ARRAY '(3) :INITIAL-CONTENTS
                                           '(230 173 163) . #1#)
                               . #2#)
                        #15# #15#))
                   (44
                    . #(#16=#.(BABEL:OCTETS-TO-STRING
                               (MAKE-ARRAY '(3) :INITIAL-CONTENTS
                                           '(232 188 137) . #1#)
                               . #2#)
                        #16# #16#))
                   (48
                    . #(#17=#.(BABEL:OCTETS-TO-STRING
                               (MAKE-ARRAY '(3) :INITIAL-CONTENTS
                                           '(230 165 181) . #1#)
                               . #2#)
                        #17# #17#))
                   (52
                    . #(#18=#.(BABEL:OCTETS-TO-STRING
                               (MAKE-ARRAY '(9) :INITIAL-CONTENTS
                                           '(230 129 146 230 178 179 230 178
                                             153)
                                           . #1#)
                               . #2#)
                        #18# #18#))
                   (56
                    . #(#19=#.(BABEL:OCTETS-TO-STRING
                               (MAKE-ARRAY '(9) :INITIAL-CONTENTS
                                           '(233 152 191 229 131 167 231 165
                                             135)
                                           . #1#)
                               . #2#)
                        #19# #19#))
                   (60
                    . #(#20=#.(BABEL:OCTETS-TO-STRING
                               (MAKE-ARRAY '(9) :INITIAL-CONTENTS
                                           '(233 130 163 231 148 177 228 187
                                             150)
                                           . #1#)
                               . #2#)
                        #20# #20#))
                   (64
                    . #(#21=#.(BABEL:OCTETS-TO-STRING
                               (MAKE-ARRAY '(12) :INITIAL-CONTENTS
                                           '(228 184 141 229 143 175 230 128
                                             157 232 173 176)
                                           . #1#)
                               . #2#)
                        #21# #21#))
                   (68
                    . #(#22=#.(BABEL:OCTETS-TO-STRING
                               (MAKE-ARRAY '(12) :INITIAL-CONTENTS
                                           '(231 132 161 233 135 143 229 164
                                             167 230 149 176)
                                           . #1#)
                               . #2#)
                        #22# #22#))
                   (-1
                    . #(#23=#.(BABEL:OCTETS-TO-STRING
                               (MAKE-ARRAY '(3) :INITIAL-CONTENTS
                                           '(229 136 134) . #1#)
                               . #2#)
                        #23# #23#))
                   (-2
                    . #(#24=#.(BABEL:OCTETS-TO-STRING
                               (MAKE-ARRAY '(3) :INITIAL-CONTENTS
                                           '(229 142 152) . #1#)
                               . #2#)
                        #24#
                        #.(BABEL:OCTETS-TO-STRING
                           (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(233 135 144)
                                       . #1#)
                           . #2#)))
                   (-3
                    . #(#25=#.(BABEL:OCTETS-TO-STRING
                               (MAKE-ARRAY '(3) :INITIAL-CONTENTS
                                           '(230 175 155) . #1#)
                               . #2#)
                        #25#
                        #.(BABEL:OCTETS-TO-STRING
                           (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(230 175 171)
                                       . #1#)
                           . #2#)))
                   (-4
                    . #(#26=#.(BABEL:OCTETS-TO-STRING
                               (MAKE-ARRAY '(3) :INITIAL-CONTENTS
                                           '(231 179 184) . #1#)
                               . #2#)
                        #26#
                        #.(BABEL:OCTETS-TO-STRING
                           (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(231 181 178)
                                       . #1#)
                           . #2#)))
                   (-5
                    . #(#27=#.(BABEL:OCTETS-TO-STRING
                               (MAKE-ARRAY '(3) :INITIAL-CONTENTS
                                           '(229 191 189) . #1#)
                               . #2#)
                        #27# #27#))
                   (-6
                    . #(#28=#.(BABEL:OCTETS-TO-STRING
                               (MAKE-ARRAY '(3) :INITIAL-CONTENTS
                                           '(229 190 174) . #1#)
                               . #2#)
                        #28# #28#))
                   (-7
                    . #(#29=#.(BABEL:OCTETS-TO-STRING
                               (MAKE-ARRAY '(3) :INITIAL-CONTENTS
                                           '(231 185 138) . #1#)
                               . #2#)
                        #29# #29#))
                   (-8
                    . #(#30=#.(BABEL:OCTETS-TO-STRING
                               (MAKE-ARRAY '(3) :INITIAL-CONTENTS
                                           '(230 178 153) . #1#)
                               . #2#)
                        #30# #30#))
                   (-9
                    . #(#31=#.(BABEL:OCTETS-TO-STRING
                               (MAKE-ARRAY '(3) :INITIAL-CONTENTS
                                           '(229 161 181) . #1#)
                               . #2#)
                        #31# #31#))
                   (-10
                    . #(#32=#.(BABEL:OCTETS-TO-STRING
                               (MAKE-ARRAY '(3) :INITIAL-CONTENTS
                                           '(229 159 131) . #1#)
                               . #2#)
                        #32# #32#))
                   (-11
                    . #(#33=#.(BABEL:OCTETS-TO-STRING
                               (MAKE-ARRAY '(3) :INITIAL-CONTENTS
                                           '(230 184 186) . #1#)
                               . #2#)
                        #33# #33#))
                   (-12
                    . #(#34=#.(BABEL:OCTETS-TO-STRING
                               (MAKE-ARRAY '(3) :INITIAL-CONTENTS
                                           '(230 188 160) . #1#)
                               . #2#)
                        #34# #34#))
                   (-13
                    . #(#35=#.(BABEL:OCTETS-TO-STRING
                               (MAKE-ARRAY '(6) :INITIAL-CONTENTS
                                           '(230 168 161 231 179 138) . #1#)
                               . #2#)
                        #35# #35#))
                   (-14
                    . #(#36=#.(BABEL:OCTETS-TO-STRING
                               (MAKE-ARRAY '(6) :INITIAL-CONTENTS
                                           '(233 128 161 229 183 161) . #1#)
                               . #2#)
                        #36# #36#))
                   (-15
                    . #(#37=#.(BABEL:OCTETS-TO-STRING
                               (MAKE-ARRAY '(6) :INITIAL-CONTENTS
                                           '(233 160 136 232 135 190) . #1#)
                               . #2#)
                        #37# #37#))
                   (-16
                    . #(#38=#.(BABEL:OCTETS-TO-STRING
                               (MAKE-ARRAY '(6) :INITIAL-CONTENTS
                                           '(231 158 172 230 129 175) . #1#)
                               . #2#)
                        #38# #38#))
                   (-17
                    . #(#39=#.(BABEL:OCTETS-TO-STRING
                               (MAKE-ARRAY '(6) :INITIAL-CONTENTS
                                           '(229 188 190 230 140 135) . #1#)
                               . #2#)
                        #39# #39#))
                   (-18
                    . #(#40=#.(BABEL:OCTETS-TO-STRING
                               (MAKE-ARRAY '(6) :INITIAL-CONTENTS
                                           '(229 136 185 233 130 163) . #1#)
                               . #2#)
                        #40# #40#))
                   (-19
                    . #(#41=#.(BABEL:OCTETS-TO-STRING
                               (MAKE-ARRAY '(6) :INITIAL-CONTENTS
                                           '(229 133 173 229 190 179) . #1#)
                               . #2#)
                        #41# #41#))
                   (-20
                    . #(#42=#.(BABEL:OCTETS-TO-STRING
                               (MAKE-ARRAY '(6) :INITIAL-CONTENTS
                                           '(232 153 154 231 169 186) . #1#)
                               . #2#)
                        #42# #42#))
                   (-21
                    . #(#43=#.(BABEL:OCTETS-TO-STRING
                               (MAKE-ARRAY '(6) :INITIAL-CONTENTS
                                           '(230 184 133 230 181 132) . #1#)
                               . #2#)
                        #43# #43#)))
                 :TEST 'EQUALP :DOCUMENTATION
                 "An alist of (<power> . (<normal> <formal> <old>))")

(DEFINE-CONSTANT +POWER-MAX+ 68 :TEST 'EQUALP :DOCUMENTATION NIL)

(DEFINE-CONSTANT +POWER-MIN+ -21 :TEST 'EQUALP :DOCUMENTATION NIL)

(DEFINE-CONSTANT +MINUS-SIGN+
                 #(#.(BABEL:OCTETS-TO-STRING
                      (MAKE-ARRAY '(12) :INITIAL-CONTENTS
                                  '(227 131 158 227 130 164 227 131 138 227 130
                                    185)
                                  . #1=(:ELEMENT-TYPE '(UNSIGNED-BYTE 8)))
                      . #2=(:ENCODING :UTF-8))
                   #.(BABEL:OCTETS-TO-STRING
                      (MAKE-ARRAY '(6) :INITIAL-CONTENTS
                                  '(232 178 160 227 129 174) . #1#)
                      . #2#)
                   #.(BABEL:OCTETS-TO-STRING
                      (MAKE-ARRAY '(6) :INITIAL-CONTENTS
                                  '(232 178 160 228 185 139) . #1#)
                      . #2#)
                   #.(BABEL:OCTETS-TO-STRING
                      (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(226 136 146) . #1#)
                      . #2#))
                 :TEST 'EQUALP :DOCUMENTATION
                 "A vector of (<normal> <formal> <old> <positional>")

(DEFINE-CONSTANT +FRACTION-PARTS-OF+
                 #(#1=#.(BABEL:OCTETS-TO-STRING
                         (MAKE-ARRAY '(6) :INITIAL-CONTENTS
                                     '(229 136 134 227 129 174)
                                     . #2=(:ELEMENT-TYPE '(UNSIGNED-BYTE 8)))
                         . #3=(:ENCODING :UTF-8))
                   #1#
                   #.(BABEL:OCTETS-TO-STRING
                      (MAKE-ARRAY '(6) :INITIAL-CONTENTS
                                  '(229 136 134 228 185 139) . #2#)
                      . #3#)
                   #.(BABEL:OCTETS-TO-STRING
                      (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(239 188 143) . #2#)
                      . #3#))
                 :TEST 'EQUALP :DOCUMENTATION
                 "A vector of (<normal> <formal> <old> <positional>")

(DEFINE-CONSTANT +RADIX-POINT+
                 #(#1=#.(BABEL:OCTETS-TO-STRING
                         (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(227 131 187)
                                     :ELEMENT-TYPE '(UNSIGNED-BYTE 8))
                         :ENCODING :UTF-8)
                   #1# #1# #1#)
                 :TEST 'EQUALP :DOCUMENTATION
                 "A vector of (<normal> <formal> <old> <positional>")

(DEFINE-CONSTANT +INFINITE+
                 #(#1=#.(BABEL:OCTETS-TO-STRING
                         (MAKE-ARRAY '(9) :INITIAL-CONTENTS
                                     '(231 132 161 233 153 144 229 164 167)
                                     :ELEMENT-TYPE '(UNSIGNED-BYTE 8))
                         :ENCODING :UTF-8)
                   #1# #1# #1#)
                 :TEST 'EQUALP :DOCUMENTATION
                 "A vector of (<normal> <formal> <old> <positional>")

(DEFINE-CONSTANT +NAN+
                 #(#1=#.(BABEL:OCTETS-TO-STRING
                         (MAKE-ARRAY '(6) :INITIAL-CONTENTS
                                     '(233 157 158 230 149 176) :ELEMENT-TYPE
                                     '(UNSIGNED-BYTE 8))
                         :ENCODING :UTF-8)
                   #1# #1# #1#)
                 :TEST 'EQUALP :DOCUMENTATION
                 "A vector of (<normal> <formal> <old> <positional>")

(DEFINE-CONSTANT +SEN+
                 #.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(233 138 173)
                                :ELEMENT-TYPE '(UNSIGNED-BYTE 8))
                    :ENCODING :UTF-8)
                 :TEST 'EQUALP :DOCUMENTATION NIL)

(DEFINE-CONSTANT +WARI+
                 #.(BABEL:OCTETS-TO-STRING
                    (MAKE-ARRAY '(3) :INITIAL-CONTENTS '(229 137 178)
                                :ELEMENT-TYPE '(UNSIGNED-BYTE 8))
                    :ENCODING :UTF-8)
                 :TEST 'EQUALP :DOCUMENTATION NIL)
