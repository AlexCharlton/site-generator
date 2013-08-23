;;;; package.lisp

(defpackage #:site-generator
  (:nicknames :sg)
  (:use #:cl #:iterate #:let-plus #:alexandria #:cl-fad #:cl-ppcre #:bordeaux-threads)
  (:shadowing-import-from #:cl-fad
			  #:copy-file #:copy-stream))

