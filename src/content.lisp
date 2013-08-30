(in-package :site-generator)
;;;; ## Site content
;;;; The site's content (and configuration) is parsed from text files (a.k.a. content files). Lines beginning with a keyword denote a variable, and the next lines before the next keyword are the content of that variable. Additionally, keywords may have further key-value pairs, strings separated by equals (e.g. foo=bar), after them.

;;;; While the syntax is the same, there are really two sorts of variables that are stored in content files, content and configuration. The combination of the two will be referred to, regrettably, as "data".

;;;; Content is the text that can be inserted into a template. As such, it can be configured to behave in a number of ways through the use of additional arguments. For example:
;;;; :content markup=markdown
;;;; Hello world!
;;;; Associates the string "Hello world!" with the variable `content`. Additionally, this content will be interpreted as markdown. Content is segregated into languages, with each piece of language-associated content able to have a different configuration. In the previous example, the content "Hello world!", as well as the fact that it is markdown, would be associated with :EN, the default language. A new piece of data:
;;;; :content lang=fr markup=none
;;;; Salut, monde!
;;;; Would associate the string "Salut, monde!" again with the variable `content`, but this time with the language :FR, and the markup is understood to be none. If no content is associated with a given variable in any language other than the default, when other languages are printed, the content associated with the default language will be used.

;;;; Configuration, on the other hand, exists the same in every language. Their interpretation is not modified by any arguments so key-value pairs on the same line as the configuration variable declaration are ignored. Depending on the variable, the lines following will be interpreted differently. A detailed explanation of the configuration variables can be seen in the README.

;;;; Two pieces of configuration are unlike the others in that they also have languages associated with them: :SLUG and :DIRECTORY-SLUG. These are the strings that the URLS for a given page are created with, :SLUG being the URL-name of an actual page, while :DIRECTORY-SLUG is the URL-name of the directory in which pages reside. Consequentially, they are parsed like normal content, despite their special meaning.
(export
 '(parse-content
   markup
   with-environment
   *environment*
   get-data))

;;;; Data is named and stored in a plist referred to as an "environment". Data appears either in the form:
;;;; Value
;;;; Or
;;;; ({Language-keyword (content &rest arguments)}+)
;;;; The former referring to a piece of configuration, while the latter refers to a piece of content (or a slug).

(defparameter *environment*
  '(:languages (:en)
    :default-language :en
    :default (:content (:markup :markdown))
    :pages-as-directories :true
    :output-format :html5
    :markup :none
    :smart :true
    :highlight :true
    :use (:cl :site-generator :cl-who))
  "The default environment.")

(defparameter *config-vars*
  `(:template ,#'first-line
    :languages ,#'keywords
    :default-language ,#'string->keyword
    :use ,#'keywords
    :pages-as-directories ,#'string->keyword
    :output-format ,#'string->keyword
    :markup ,#'string->keyword
    :smart ,#'string->keyword
    :toc ,#'string->keyword
    :toc-depth ,#'string->keyword
    :highlight ,#'string->keyword
    :math ,#'string->keyword
    :number-sections ,#'string->keyword
    :default ,#'(lambda (x)
		  (mapcan (lambda (line)
	   		    (let+ (((var &rest args) (is-variable? line)))
			      (list var args)))
			  (lines x)))
    :server ,#'first-line
    :extension ,#'(lambda (x) (first (words x)))
    :depends ,#'(lambda (x) (mapcar #'trim (lines x)))
    :cl-environment ,#'identity
    :commands ,#'escaped-lines
    :date ,#'parse-date
    :slug nil
    :directory-slug nil
    :lang t
    :current-file t)
"The list of variables that are related to configuration and are not treated as content, i.e. they are not considered to belong to a language and will not be marked up. Variables set to nil will be parsed like content and are present to ensure that these variables can be excluded from content.")

(defun get-language (env)
  "Plist -> Keyword
Return currently active language."
  (or (getf env :lang)
      (getf *environment* :default-language)))

(defun destructure-data (name data &optional lang)
  "Keyword Data &optional Keyword -> (values Data Type Args)
Where Type is a keyword and Args is a plist.

Given the DATA with NAME, determine the type of data and return its value, with args (defaulting to defaults) if applicable."
  (if (getf *config-vars* name)
	(values data :config)
	(let* ((lang (or lang (get-language *environment*)))
	       (data (or (getf data lang)
			 (getf data (getf *environment* :default-language))))
	       (defaults (getf (getf *environment* :default) name)))
	  (when lang
	    (values (first data) :content (merge-plists (rest data) defaults))))))

(defun get-data (name &optional lang)
  "Keyword Plist &optional Keyword Plist -> (values Data Type Args) Or nil
Where Type is a keyword and Args is a plist.

Given the NAME, return the piece of configuration or content data from ENV."
  (let ((lang (or lang (get-language *environment*)))
	(data (getf *environment* name)))
    (when data
      (destructure-data name data lang))))

(defun merge-environments (env-new env-base)
  "Plist Plist -> Plist
Create a fresh plist, made by combining ENV1 with ENV2, with ENV1 taking precedence.

The exception to these merging rules is for the element :DEFAULT, which is merged with any old :DEFAULTs."
  (let ((env-base (copy-list env-base)))
    (iter (for (k v) on env-new by #'cddr)
	  (setf (getf env-base k)
		(case k
		  (:default (merge-defaults v (getf env-base k)))
		  (otherwise v))))
    env-base))

(defun merge-defaults (default-new default-base)
  "Plist Plist -> Plist
Merge two :DEFAULT plists."
  (let ((base (copy-list default-base)))
    (iter (for (k v) on default-new by #'cddr)
	  (setf (getf base k)
		(merge-plists v (getf base k))))
    base))

;;;; ## Evaluation environment
(defmacro with-environment (&body body)
  "Evaluate the BODY in a semi anonymous package, filled with content values associated with *ENVIRONTMENT*. Content is chosen based on :LANG, and the packages :USEs the list :USE in *ENVIRONTMENT*."
  `(let ((*package* (defpackage ,(gensym "environment")
		      ,(cons :use (getf *environment* :use)))))
     (handler-bind ((warning #'muffle-warning))
       (set-up-content-environment)
       (set-up-cl-environment)
       ,@body
       (delete-package *package*))))

(defun set-up-content-environment ()
  "For each peice of content in *environment* (see DESTRUCTURE-DATA), set the value of that symbol to the content, and set the macro-function of that symbol to a call to markup and recursively expand the content."
  (iter (for (var val) on *environment* by #'cddr)
	(let+ (((&values data type args) (destructure-data var val)))
	  (when (eq type :content)
	    (setf (symbol-value (intern (symbol-name var)))
		  data
		  (macro-function (intern (symbol-name var)))
		  (lambda (call env)
		    (declare (ignore call env))
		    (append (list 'markup (expand-string data))
			    args)))))))

(defun set-up-cl-environment ()
  "Evaluate each expression of the :cl-environment value of *ENVIRONMENT*."
  (when-let ((cl-env (get-data :cl-environment)))
    (with-input-from-string (s cl-env)
      (iter (for expr = (read s nil 'eof))
	    (until (eq expr 'eof))
	    (eval expr)))))

;;;; ## Parse content files
(defvar *variable-scanner* (create-scanner "^:(\\S+)(.*)")
  "Match a 'variable'.")

(defun parse-content (file)
  "Pathname -> Plist
Return a plist of the Name-Data pairs from FILE."
  (let (ret)
    (iter (for (var args content) on (parse-content-file file) by #'cdddr)
	  (let ((content (trim content)))
	    (when (string= content "")
	      (error "Variable ~a does not have any content in file ~a"
		     var file))
	    (if-let ((config-fn (getf *config-vars* var)))
		(setf (getf ret var) (funcall config-fn content))
		(let ((lang (get-language args))
		      (other-content (or (getf ret var)
					 nil)))
		  (remf args :lang)
		  (setf (getf ret var)
			(append (list lang (cons content args))
				other-content))))))
    ret))

(defun parse-content-file (file)
  "pathname -> list
Given a FILE, parse that file into a list representing the different components of that content file, namely keyword variables, arguments supplied to them, and their content.

Keyword variables are deliniated by a blank newline before IS-VARIABLE? matches."
  (let (ret
	(last-line-blank? t))
    (with-open-file (f file)
      (iter (for line = (read-line f nil 'eof))
	    (until (eq line 'eof))
	    (let ((variable? (is-variable? line)))
	      (if (and variable? last-line-blank?)
		  (let+ (((variable &rest args) variable?))
		    (push variable ret)
		    (push args ret)
		    (push "" ret))
		  (when (string/= (trim line) "")
		    (if (stringp (car ret))
			(setf (car ret) (concatenate 'string (car ret) line
						     '(#\Newline)))
			(error "Not a well-formed content file: ~a. No initial variable found." file)))))
	    (setf last-line-blank? (string= (trim line) ""))))
    (reverse ret)))

(defun is-variable? (line)
  "String -> (keyword &rest args) OR nil
Returns a keyword and a list of keyword arguments if the line begins with a keyword."
  (register-groups-bind (var args)
      (*variable-scanner* line)
    (let ((args (parse-args args)))
      (if (evenp (length args))
	  (cons (string->keyword var) args)
	  (error "Odd number of keyword arguments to variable ~a: ~s"
		 var line)))))

(defun parse-args (args)
  "String -> (Keyword)
Break a string by spaces and equals signs and return a list of the resulting strings, in keyword from."
  (mapcar #'string->keyword
   (mapcan (lambda (x) 
	     (split "=" x))
	   (words args))))


;;; ### Mark up content
(defvar *pandoc-supported-args*
  `(:output-format ,(lambda (x) (format nil "--to=~(~a~)" x))
    :markup ,(lambda (x) (format nil "--from=~(~a~)" x))
    :smart ,(lambda (x) (when (eq x :true) "--smart"))
    :toc ,(lambda (x) (when (eq x :true) "--toc"))
    :toc-depth ,(lambda (x) (format nil "--toc-depth=~(~a~)" x))
    :highlight ,(lambda (x) (when (not (eq x :true)) "--no-highlight"))
    :math ,(lambda (x) (when (eq x :true) "--mathjax"))
    :number-sections ,(lambda (x) (when (eq x :true) "--number-sections")))
  "Maps the arguments supported by site-generator to the string arguments supported by pandoc.")

(defun generate-pandoc-args (args)
  "(Plist) -> String
Turn a Plist of arguments and *ENVIRONMENT* into a string understood by pandoc. *PANDOC-SUPPORTED-ARGS* is used for this mapping."
  (apply #'join-strings " "
   (iter (for (supported-arg fn) on *pandoc-supported-args* by #'cddr)
	 (if-let ((val (or (getf args supported-arg)
			   (getf *environment* supported-arg))))
	   (collect (funcall fn val))))))

(defun pandoc-process (string &optional args)
  "String &optional (Plist) -> String
Pass the given STRING and ARGS through pandoc given *ENVIRONMENT*.

When :TOC is set, call GET-PANDOC-TOC and replace any strings of {{{toc}}} (that aren't preceded by <code> tags) that appear in the text, with the results. Also set 'TOC."
  (with-open-temporary-file (s :direction :output)
    (iter (for char in-string string)
	  (write-char char s))
    (let ((toc (when (or (getf args :toc)
			 (getf *environment* :doc))
		 (get-pandoc-toc s args)))
	  (output (progn
		    (file-position s 0)
		    (trim (asdf/interface::run-program 
			   (join-strings " " "pandoc"
					 (generate-pandoc-args args)
					 (namestring (pathname s)))
			   :output :string)))))
      (if toc
	  (progn
	    (setf (symbol-value (intern "toc" *package*)) toc)
	    (regex-replace-all
	    "<p>\\{\\{\\{toc\\}\\}\\}</p>|(?<!<code>)\\{\\{\\{toc\\}\\}\\}"
	    output
	    toc))
	  output))))

#+sbcl
(defun get-pandoc-toc (file-stream args)
  "Pass the file through Pandoc with a special table-of-contents-only template in order to determine the table of contents.

If the template doesn't exist, create it."
  (let+ (((&flet create-toc-template (dir)
	    (print-message "No Pandoc toc template. Creating at ~a (root permission may be necessary)" dir)
	    (with-open-file (s (merge-pathnames "toc.html" dir)
			       :direction :output :if-exists :supersede)
	      (write-line "$toc$" s))
	    (with-open-file (s (merge-pathnames "toc.html5" dir)
			       :direction :output :if-exists :supersede)
	      (write-line "$toc$" s)))))
    (file-position file-stream 0)
    (let ((toc (let ((proc (sb-ext:run-program
			    "pandoc"
			    (concatenate 'list 
					 (words (generate-pandoc-args args))
					 (list "--standalone"
					       "--template=toc"
					       (namestring (pathname file-stream))))
			    :output :stream :search t)))
		 (with-output-to-string (out)
		   (iter (for line = (read-line (sb-ext:process-output proc)
						nil 'eof))
			 (until (eq line 'eof))
			 (princ line out))
		   (sb-ext:process-close proc)))))
      (if (register-groups-bind (dir)
	      ("^pandoc: (.+)toc.html5?" toc)
	    (create-toc-template dir))
	  (error "Fuuuuuu")
	  ;(get-pandoc-toc file-stream args)
	  toc))))

#-sbcl
(defun get-pandoc-toc (file-stream args)
  "Pass the file through Pandoc with a special table-of-contents-only template in order to determine the table of contents.

If the template doesn't exist, create it. This function must prompt for the path to the template directory, so it is not the prefered method."

  (let+ (((&flet create-toc-template ()
	    (format t "Pandoc does not have a toc template.~%Please enter the location of the Pandoc template directory (blank line cancels):~%")
	    (let ((dir (read-line)))
	      (unless (string= dir "")
		(with-open-file (s (merge-pathnames "toc.html"
						    (pathname-as-directory dir))
				   :direction :output :if-exists :supersede)
		  (write-line "$toc$" s))
		(with-open-file (s (merge-pathnames "toc.html5"
						    (pathname-as-directory dir))
				   :direction :output :if-exists :supersede)
		  (write-line "$toc$" s))
		(get-pandoc-toc file-stream args))))))
    (file-position file-stream 0)
    (handler-case
	(asdf/interface::run-program 
	 (join-strings " " "pandoc"
		       "--standalone"
		       "--template=toc"
		       (generate-pandoc-args args)
		       (namestring (pathname file-stream)))
	 :output :string)
    (asdf/run-program:subprocess-error () (create-toc-template)))))

(defun markup (content &rest args)
  "String &rest (Key Value) -> String
Unless, :MARKUP in ARGS or *ENVIRONMENT* is :NONE, process content using pandoc."
  (let ((markup (or (getf args :markup)
		    (getf *environment* :markup))))
    (if (or (eq markup :none)
	    (string= content ""))
	content
	(pandoc-process content args))))
