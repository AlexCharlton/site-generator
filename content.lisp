(in-package :site-generator);;;; ## Site content
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
   process-content
   *environment*))

;;;; Data is named and stored in a plist referred to as an "environment". Data appears either in the form:
;;;; Value
;;;; Or
;;;; ({Language-keyword (content &rest arguments)}+)
;;;; The former referring to a piece of configuration, while the latter refers to a piece of content (or a slug).

(defvar *environment*
  '(:languages (:en)
    :default-language :en
    :pages-as-directories :true
    :output-format :html5
    :markup :none
    :smart :true
    :toc :false
    :highlight :true
    :use (:cl :site-generator :iterate))
  "The default environment.")

(defvar *config-vars*
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
    :highlight-style ,#'string->keyword
    :math ,#'string->keyword
    :default ,#'(lambda (x)
		  (mapcan (lambda (line)
			    (let+ (((var &rest args) (is-variable? line)))
			      (list var args)))
			  (lines x)))
    :slug nil
    :directory-slug nil
    :lang t)
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

(defun get-data (name &optional lang (env *environment*))
  "Keyword Plist &optional Keyword Plist -> (values Data Type Args) Or nil
Where Type is a keyword and Args is a plist.

Given the NAME, return the piece of configuration or content data from ENV."
  (let ((lang (or lang (get-language env)))
	(data (getf env name)))
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

;;;; ## Parse content files
(defvar *variable-scanner* (create-scanner "^:(\\S+)(.*)")
  "Match a 'variable'.")

(defun parse-content (file)
  "Pathname -> Plist
Return a plist of the Name-Data pairs from FILE."
  (let (ret)
    (iter (for (var args content) on (parse-content-file file) by #'cdddr)
	  (let ((content (trim-whitespace content)))
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
		  (if (stringp (car ret))
		      (setf (car ret) (concatenate 'string (car ret) line
						   '(#\Newline)))
		      (error "Not a well-formed content file: ~a. No initial variable found." file))))
	    (setf last-line-blank? (string= line ""))))
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
    :highlight-style ,(lambda (x) (format nil "--highlight-style=~(~a~)" x))
    :math ,(lambda (x) (when (eq x :true) "--mathjax")))
  "Maps the arguments supported by site-generator to the string arguments supported by pandoc.")

(defun generate-pandoc-args (args)
  "(Plist) -> String
Turn a Plist of arguments and *ENVIRONMENT* into a string understood by pandoc. *PANDOC-SUPPORTED-ARGS* is used for this mapping."
  (join-string-list
   (iter (for (supported-arg fn) on *pandoc-supported-args* by #'cddr)
	 (if-let ((val (or (getf args supported-arg)
			   (getf *environment* supported-arg))))
	   (collect (funcall fn val))))))

(defun pandoc-process (string &optional args)
  "String &optional (Plist) -> String
Pass the given STRING and ARGS through pandoc given *ENVIRONMENT*."
  (with-open-temporary-file (s :direction :output)
    (iter (for char in-string string)
	  (write-char char s))
    (file-position s 0)
    (inferior-shell:run/ss
     (join-string-list (list "pandoc"
			     (generate-pandoc-args args)
			     (namestring (pathname s)))))))

(defun process-content (content &rest args)
  "String &rest (Key Value) -> String
Unless, :MARKUP in ARGS or *ENVIRONMENT* is :NONE, process content using pandoc."
  (let ((markup (or (getf args :markup)
		    (getf *environment* :markup))))
    (if (or (eq markup :none)
	    (string= content ""))
	content
	(pandoc-process content args))))
