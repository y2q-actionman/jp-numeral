(defsystem :jp-numeral-test
  :description "Tests for jp-numeral."
  :license "MIT"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on (:jp-numeral :alexandria)
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
     (:file "wari")
     (:file "yen")
     (:file "all"))))
  :perform (test-op (o s)
  		    (symbol-call '#:jp-numeral.test '#:main)))
