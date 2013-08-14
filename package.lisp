;;;; package.lisp

(defpackage #:site-generator
  (:nicknames :sg)
  (:use #:cl #:iterate #:let-plus #:alexandria #:cl-fad #:cl-ppcre)
  (:shadowing-import-from #:cl-fad
			  #:copy-file #:copy-stream))

