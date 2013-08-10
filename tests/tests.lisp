(in-package :site-generator-test)
(in-suite :site-generator)

(test templates
  (with-env ('(:bar "baz"))
    (is (string= "Foo baz."
		 (expand-string-to-string "Foo $bar$.")))
    (is (string= "Foo baz quox."
		 (expand-string-to-string "Foo $(concatenate 'string bar \" quox\").")))
    (is (string= "$ hello"
		 (expand-string-to-string "$ hello")))
    (is (string= "$hello there$"
		 (expand-string-to-string "$hello there$")))
    (is (string= "$hello"
		 (expand-string-to-string "\\$hello")))
    (is (string= "\\baz"
		 (expand-string-to-string "\\\\$bar$")))
    (is (string= "\\$hello"
		 (expand-string-to-string "\\\\\\$hello")))
    (is (string= "\\\\ hello"
		 (expand-string-to-string "\\\\ hello")))
    (is (string= "\\\\ baz"
		 (expand-string-to-string "\\\\ $bar$")))))
