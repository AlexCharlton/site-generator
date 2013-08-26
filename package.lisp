;;;; package.lisp

(defpackage #:site-generator
  (:nicknames :sg)
  (:use #:cl #:iterate #:let-plus #:alexandria #:cl-fad #:cl-ppcre #:bordeaux-threads #:cl-who #:local-time)
  (:import-from #:osicat
		#:make-link #:delete-directory #:file-kind #:unmerge-pathnames)
  (:shadowing-import-from #:cl-fad
			  #:copy-file #:copy-stream))

