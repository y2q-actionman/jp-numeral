(defsystem :jp-numeral.table-generator
  :description "A script for making jp-numeral."
  :license "MIT"
  :author "YOKOTA Yuki <y2q.actionman@gmail.com>"
  :depends-on (:alexandria :babel)
  :components
  ((:file "table-generator")))

;;; Usage:
;;; 
;;; To make 'jp-numeral-table.lisp', do below after loading
;;; the DEFPACKAGE of 'jp-numeral'.
;;; 
;;; (asdf:load-asd "table-generator.asd")
;;; (asdf:load-system :jp-numeral.table-generator)
;;; (jp-numeral.table-generator:generate-file "/tmp/jp-numeral-table.lisp" (find-package "JP-NUMERAL"))
