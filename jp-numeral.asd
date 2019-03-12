(defsystem :jp-numeral
  :description "A printer for Japanese numerals."
  :license "MIT"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on (:alexandria :babel)
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "package")
     (:file "jp-numeral-table")
     (:file "jp-numeral"))))
  :in-order-to ((test-op (test-op #:jp-numeral-test))))
