(in-package :site-generator-test)
(in-suite :site-generator)

(defvar *test-dir* (merge-pathnames "tests/"
				    (asdf:component-pathname
				     (asdf:find-system :site-generator))))

(test templates
  (with-env ('(:bar "baz"
	       :foo "$bar$"
	       :baz "$foo$"))
    (is (string= "Foo baz."
		 (expand-string-to-string "Foo $bar$.")))
    (is (string= "Foo baz quox."
		 (expand-string-to-string "Foo $(concatenate 'string bar \" quox\").")))
    (is (string= "Foo baz."
		 (expand-string-to-string "Foo $foo$." :recursive-expansion t)))
    (is (string= "Foo baz."
		 (expand-string-to-string "Foo $baz$." :recursive-expansion t)))
    (is (string= "Foo $bar$."
		 (expand-string-to-string "Foo $foo$.")))
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

(test content-parsing
  (is (equal '(:foo)
	     (sg::is-variable? ":foo")))
  (is (equal '(:foo :bar :baz)
	     (sg::is-variable? ":foo bar=baz")))
  (signals simple-error
    (sg::is-variable? ":foo bar"))
  (signals simple-error
    (sg::is-variable? ":foo bar=baz quox"))
  (is (equal
       '(:SITE-NAME NIL "site-generator
"
	 :TEMPLATE NIL "main.html
"
	 :NAV (:LANG :EN) "* [Home]() * [About site-generator]($(address \"/about\"))
"
	 :NAV (:LANG :FR)
	 "* [Accueil]() * [À propos de site-generator]($(address \"/about\"))
"
	 :FOOTER-TEXT (:LANG :EN)
	 "This is the end of the page$(when title (write \" \\\"\" title \"\\\" (these should be curly quotes)\"). 
"
	 :FOOTER-TEXT (:LANG :FR)
	 "Ceci est la fin de la page$(when title (write \" \\\"\" title \"\\\"\").")
       (sg::parse-content-file (merge-pathnames "examples/example-site/global" *test-dir*))))
  (is (equal
       '(:FOOTER-TEXT
	 (:FR
	  "Ceci est la fin de la page$(when title (write \" \\\"\" title \"\\\"\")."
	  :EN
	  "This is the end of the page$(when title (write \" \\\"\" title \"\\\" (these should be curly quotes)\").")
	 :NAV
	 (:FR "* [Accueil]() * [À propos de site-generator]($(address \"/about\"))"
	  :EN "* [Home]() * [About site-generator]($(address \"/about\"))")
	 :TEMPLATE "main.html" :SITE-NAME (:EN "site-generator"))
       (parse-content(merge-pathnames "examples/example-site/global" *test-dir*))))
  (signals simple-error
    (parse-content (merge-pathnames "bad-content1" *test-dir*)))
  (signals simple-error
    (parse-content (merge-pathnames "bad-content2" *test-dir*)))
  (is (string= "--to=html5 --from=markdown --toc"
	       (sg::generate-pandoc-args '(:markup :markdown :toc :true :smart :false))))
  (is (string= "<h1 id=\"foo\">foo</h1>"
	       (process-content "# foo" '())))
  (is (string= "foo"
	       (process-content "foo" '(:markup "raw")))))
