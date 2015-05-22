;; Copyright (c) 2015 YOKOTA Yuki <y2q.actionman@gmail.com>
;;
;; This software is released under the MIT License.
;; See the LICENSE file.

(in-package :cl-user)

(asdf:defsystem :jp-numeral.table-generator
  :description "A script for making jp-numeral."
  :license "MIT"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on (:babel)
  :components
  ((:file "table-generator")))
