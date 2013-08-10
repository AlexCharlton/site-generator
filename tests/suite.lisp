(defpackage :site-generator-test
  (:use :cl :sg :fiveam :let-plus))

(in-package :site-generator-test)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-suite :site-generator))
