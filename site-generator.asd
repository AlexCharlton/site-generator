;;;; site-generator.asd

(asdf:defsystem #:site-generator
  :serial t
  :description "Describe site-generator here"
  :author "Alex Charlton <alex.n.charlton@gmail.com>"
  :license "BSD-2"
  :depends-on (:let-plus :alexandria :iterate :hunchentoot :com.dvlsoft.clon :inferior-shell)
  :components ((:file "package")
	       (:file "templates")
	       (:file "publish")
	       (:file "preview")
	       (:file "locales")
	       (:file "config")
	       (:file "markup")
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
