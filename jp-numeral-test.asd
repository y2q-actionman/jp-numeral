(defsystem :jp-numeral-test
  :description "Tests for jp-numeral."
  :license "MIT"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on (:jp-numeral :alexandria :1am)
  :components
  ((:module "test"
    :components
    ((:file "package")
     (:file "util")
     (:file "normal" :depends-on ("util"))
     (:file "formal" :depends-on ("util"))
     (:file "old" :depends-on ("util"))
     (:file "positional" :depends-on ("util"))
     (:file "wari" :depends-on ("util"))
     (:file "yen" :depends-on ("util")))))
  :perform (prepare-op :before (o c)
             (set (find-symbol* :*tests* :1am) '()))
  :perform (test-op (o s)
		    (symbol-call '#:1am '#:run)))
