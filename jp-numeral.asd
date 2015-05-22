;; Copyright (c) 2015 YOKOTA Yuki <y2q.actionman@gmail.com>
;;
;; This software is released under the MIT License.
;; See the LICENSE file.

(in-package :cl-user)

(asdf:defsystem :jp-numeral
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
  :in-order-to ((asdf:test-op (asdf:test-op #:jp-numeral.test)))) 
