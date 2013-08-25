;;;; site-generator.asd

(asdf:defsystem #:site-generator
  :serial t
  :description "Describe site-generator here"
  :author "Alex Charlton <alex.n.charlton@gmail.com>"
  :license "BSD-2"
  :depends-on (:let-plus :alexandria :iterate :hunchentoot :com.dvlsoft.clon :cl-ppcre :cl-fad :bordeaux-threads :osicat :cl-who :local-time)
  :components ((:file "package")
	       (:file "utility")
	       (:file "content")
	       (:file "templates")
	       (:file "publish")
	       (:file "test-server")
               (:file "site-generator"))
  :in-order-to ((test-op (load-op :site-generator-test)))
  :perform (test-op :after (op c)
		    (funcall (intern (string '#:run!) :it.bese.fiveam)
			     :site-generator)))

(asdf:defsystem :site-generator-test
  :author "Alex Charlton <alex.n.charlton@gmail.com>"
  :licence "BSD-3"
  :depends-on (:fiveam)
  :pathname "tests/"
  :serial t
  :components ((:file "suite")
	       (:file "tests")))
