;; Copyright (c) 2015 YOKOTA Yuki <y2q.actionman@gmail.com>
;;
;; This software is released under the MIT License.
;; See the LICENSE file.

(in-package :cl-user)

(asdf:defsystem :jp-numeral.test
  :description "Tests for jp-numeral."
  :license "MIT"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on (:jp-numeral)
  :components
  ((:module "test"
    :serial t    
    :components
    ((:file "package")
     (:file "util")
     (:file "normal")
     (:file "formal")
     (:file "old")
     (:file "positional")
     (:file "all"))))
  :perform (asdf:test-op (o s)
  			 (uiop:symbol-call '#:jp-numeral.test '#:main))
  )

