(in-package :site-generator)
;;;; ## Site content
;;;; The site's content (and configuration) is parsed from text files. Lines beginning with a keyword denote a variable, and the next lines before the next keyword are the content of that variable. Additionally, keywords may have further key-value pairs, strings separated by equals (e.g. foo=bar), after them.
(export
 '(parse-content
   process-content))

;;;; ## Parse content files
(defvar *variable-scanner* (create-scanner "^:(\\S+)(.*)")
  "Match a 'variable'.")

(defvar *config-vars*
  '(:template)
  "The list of variables that are related to configuration and are not treated as content, i.e. they are not considered to belong to a language and will not be marked up.")

(defvar *default-language* :en
  "The language that a block of content will be assumed to be, unless otherwise stated.")

(defun parse-args (args)
  "String -> (Keyword)
Break a string by spaces and equals signs and return a list of the resulting strings, in keyword from."
  (mapcar (lambda (x)
	    (intern (string-upcase x) :keyword))
   (mapcan (lambda (x) 
	     (split "=" x))
	   (words args))))

(defun is-variable? (line)
  "String -> (keyword &rest args) OR nil
Returns a keyword and a list of keyword arguments if the line begins with a keyword."
  (register-groups-bind (var args)
      (*variable-scanner* line)
    (let ((args (parse-args args)))
      (if (evenp (length args))
	  (cons (intern (string-upcase var) :keyword)
		args)
	  (error "Odd number of keyword arguments to variable ~a: ~s"
		 var line)))))

(defun parse-content (file)
  "pathname -> plist
Return a plist of the key value pairs from FILE."
  (let (ret)
    (iter (for (var args content) on (parse-content-file file) by #'cdddr)
	  (let ((content (trim-whitespace content)))
	    (when (string= content "")
	      (error "Variable ~a does not have any content in file ~a"
		     var file))
	    (if (find var *config-vars*)
		(setf (getf ret var) content)
		(let ((lang (or (getf args :lang) *default-language*))
		      (other-content (or (getf ret var) nil)))
		  (setf (getf ret var)
			(append (list lang content)
				other-content))))))
    ret))

(defun parse-content-file (file)
  "pathname -> list
Given a FILE, parse that file into a list representing the different components of that content file, namely keyword variables, arguments supplied to them, and their content."
  (let (ret
	(last-line-blank? t))
    (with-open-file (f file)
      (iter (for line = (read-line f nil 'eof))
	    (until (eq line 'eof))
	    (when (string= line "")
	      (setf line (coerce '(#\Newline) 'string)))
	    (let ((variable? (is-variable? line)))
	      (if (and variable? last-line-blank?)
		  (let+ (((variable &rest args) variable?))
		    (push variable ret)
		    (push args ret)
		    (push "" ret))
		  (if (stringp (car ret))
		      (setf (car ret) (concatenate 'string (car ret) line))
		      (error "Not a well-formed content file: ~a. No initial variable found." file))))
	    (setf last-line-blank? (string= line (coerce '(#\Newline) 'string)))))
    (reverse ret)))

;;; ### Mark up content
(defvar *pandoc-defaults*
  '(:output-format :html5
    :markup :markdown
    :smart :true
    :toc :false
    :highlight :true)
  "Default arguments for pandoc.")

(defvar *pandoc-supported-args*
  `(:output-format ,(lambda (x) (format nil "--to=~(~a~)" x))
    :markup ,(lambda (x) (format nil "--from=~(~a~)" x))
    :smart ,(lambda (x) (when (eq x :true) "--smart"))
    :toc ,(lambda (x) (when (eq x :true) "--toc"))
    :toc-depth ,(lambda (x) (format nil "--toc-depth=~(~a~)" x))
    :highlight ,(lambda (x) (when (not (eq x :true)) "--no-highlight"))
    :highlight-style ,(lambda (x) (format nil "--highlight-style=~(~a~)" x))
    :math ,(lambda (x) (when (eq x :true) "--mathjax")))
  "Maps the arguments supported by site-generator to the string arguments supported by pandoc.")

(defun generate-pandoc-args (args)
  "(Keyword) -> String
Turn a list of arguments from keywords into a string understood by pandoc. *PANDOC-SUPPORTED-ARGS* is used for this mapping."
  (join-string-list
   (iter (for (supported-arg fn) on *pandoc-supported-args* by #'cddr)
	 (if-let ((val (or (getf args supported-arg)
			   (getf *pandoc-defaults* supported-arg))))
	   (collect (funcall fn val))))))

(defun pandoc-process (string args)
  "String (Keyword) -> String
Pass the given string through pandoc with the arguments ARGS."
  (with-open-temporary-file (s :direction :output)
    (iter (for char in-string string)
	  (write-char char s))
    (file-position s 0)
    (inferior-shell:run/ss
     (join-string-list (list "pandoc"
			     (generate-pandoc-args args)
			     (namestring (pathname s)))))))

(defun process-content (content args)
  "String (Keyword) -> String
Unless, :MARKUP in ARGS is :NONE, process content using pandoc."
  (if (equal (getf args :markup) :none)
      content
      (pandoc-process content args)))

;;; ### Utility
(defun words (string)
  "String -> (String)
Split a string by whitespace."
  (delete "" (split "(\\s+)" string) :test #'string=))

(defun join-string-list (string-list)
    " (String) -> String
Concatenates a list of strings and puts spaces between the elements. (from Common Lisp Cookbook)"
    (format nil "~{~a~^ ~}" (remove nil string-list)))

(defun trim-whitespace (string)
  "String -> String
Remove whitespace at the beginning and end of a string."
  (string-trim '(#\Space #\Newline #\Tab) string))
