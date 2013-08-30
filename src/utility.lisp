(in-package :site-generator)
;;;; ## Utility functions
(export '(xml
	  lines
	  words
	  first-line
	  trim
	  join-strings
	  echo
	  escape-dollars
	  bound?
	  +rfc+
	  build-time))

;;;; ### Files and directories
(defun touch-file (file)
  "Pathspec -> nil
Ensure that a file exists."
  (with-open-file (s file
		     :if-does-not-exist :create)))

(defun directory-minus (path dir)
  "Pathspec Pathspec -> Pathname
If DIR represents the beginning of PATH, return the parts of PATH that are not part of DIR."
  (let ((path (namestring path))
	(dir (namestring dir)))
    (pathname
     (if (equal (search dir path) 0)
	 (subseq path (length dir))
	 path))))

(defun parent-directory (path)
  "Pathspec -> String
Return the name of the directory that contains PATH."
  (let ((path (namestring path)))
    (if (equal path "")
	path
	(last-elt (split "/" path)))))

(defun hidden-p (path)
  "Pathspec -> Boolean
Return true if the path represents a hidden file."
  (scan "^\\." (pathname-name path)))

(defun cwd (&optional dir)
  "Change directory and set default pathname.

Modified from http://files.b9.com/lboot/utils.lisp"
  (cond
   ((not (null dir))
    (when (and (typep dir 'logical-pathname)
	       (translate-logical-pathname dir))
      (setq dir (translate-logical-pathname dir)))
    (when (stringp dir)
      (setq dir (parse-namestring dir)))
    #+allegro (excl:chdir dir)
    #+sbcl (sb-posix:chdir dir)
    #+clisp (#+lisp=cl ext:cd #-lisp=cl lisp:cd dir)
    #+(or cmu scl) (setf (ext:default-directory) dir)
    #+cormanlisp (ccl:set-current-directory dir)
    #+(and mcl (not openmcl)) (ccl:set-mac-default-directory dir)
    #+openmcl (ccl:cwd dir)
    #+gcl (si:chdir dir)
    #+lispworks (hcl:change-directory dir)
    (setq cl:*default-pathname-defaults* dir))
   (t
    (let ((dir
	   #+allegro (excl:current-directory)
	   #+clisp (#+lisp=cl ext:default-directory #-lisp=cl lisp:default-directory)
	   #+(or cmu scl) (ext:default-directory)
	   #+sbcl (sb-unix:posix-getcwd/)
	   #+cormanlisp (ccl:get-current-directory)
	   #+lispworks (hcl:get-working-directory)
	   #+mcl (ccl:mac-default-directory)
	   #-(or allegro clisp cmu scl cormanlisp mcl sbcl lispworks) (truename ".")))
      (when (stringp dir)
	(setq dir (parse-namestring dir)))
      dir))))


;;;; ### Strings and characters
(defun words (string)
  "String -> (String)
Split a string by whitespace."
  (delete "" (split "\\s+" string) :test #'string=))

(defun lines (string)
  "String -> (String)
Split a string by newlines."
  (delete "" (split "\\n" string) :test #'string=))

(defun first-line (string)
  "String -> String"
  (first (lines string)))

(defun join-strings (separator &rest strings)
    "(String) -> String
Concatenates a list of strings and puts SEPARATOR between the elements."
    (format nil (format nil "~~{~~a~~^~a~~}" separator) (remove nil strings)))

(defun trim (string)
  "String -> String
Remove whitespace at the beginning and end of a string."
  (string-trim '(#\Space #\Newline #\Tab) string))

(defun echo (&rest strings)
  "&rest Strings -> String
Concatenate the strings"
  (apply #'join-strings "" strings))

(defun split-comma-or-space-separated (words)
  (split "\\s*,\\s+|\\s*,|\\s+" words))

(defun whitespace-char-p (char)
  "Character -> Boolean
Return true if CHAR is a whitespace character."
  (if (find char '(#\Space #\Newline #\Tab) :test #'char=)
      t))

(defun escaped-lines (string)
  "String -> (String)
Returns the list of strings seperated by lines as per LINES, but backslashes at the end of lines escape the new line"
  (lines (regex-replace-all "\\\\\\n" string " ")))

(defun escape-dollars (string)
  "Replace any dollar signs with their escaped equivalent. Also escapes escaped dollar signgs. Does not go any deeper down the rabbit hole."
  (regex-replace-all "(?<!\\\\)\\$"
		     (regex-replace-all "\\\\\\$" string "\\\\\\\\\\$")
		     "\\$"))

;;;; ### Keywords
(defun string->keyword (s)
  (intern (string-upcase s) :keyword))

(defun keywords (string)
  "String -> (Keyword)
Returns keywords from comma or space separated string"
  (mapcar #'string->keyword
	  (split-comma-or-space-separated string)))


;;; ### Lists
(defun times (n elt)
  "Natural X -> (X)
Return a list of n ELTs."
  (iter (for x from 0 below n)
	(collect elt)))


;;; ### Plists
(defun merge-plists (plist-new plist-base)
  (let ((base (copy-list plist-base)))
    (iter (for (k v) on plist-new by #'cddr)
	  (setf (getf base k) v))
    base))


;;; ### Time
(defconstant +rfc+ +rfc-1123-format+)

(defun make-time (element-list fallback)
  (let ((unit-accessor (list :second #'timestamp-second
			     :minute #'timestamp-minute
			     :hour #'timestamp-hour
			     :day #'timestamp-day
			     :month #'timestamp-month
			     :year #'timestamp-year)))
    (iter (for (unit val) on element-list by #'cddr)
	  (for base-time first fallback then time)
	  (for time = (timestamp+ (timestamp-
				   base-time
				   (funcall (getf unit-accessor unit)
					    fallback)
				   unit)
				  val unit))
	  (finally (return time)))))

(defun parse-date (s)
  "String -> Plist
Break a string by spaces and equals signs and return a plist of the unit value pairs."
  (iter (for (unit val) on (mapcan (lambda (x) 
				     (split "=" x))
				   (split-comma-or-space-separated s))
	     by #'cddr)
	(let ((unit (string->keyword unit))
	      (val (parse-integer val :junk-allowed t)))
	  (unless (and (find unit '(:second :minute :hour :day :month :year))
		       (integerp val))
	    (error "Improper date entered: ~s" s))
	  (collect unit) (collect val))))

(defun build-time ()
  "nil -> String
Return the RFC formated time corresponding to when the function is called."
  (format-timestring nil (now) :format +rfc+))

;;;; ### XML
(defmacro xml (&body body)
  `(with-html-output-to-string
       (,(gensym) nil
	 :prologue ,(case (caar body)
			  (:html t)
			  (:rss "<?xml version='1.0' encoding='utf-8'?>")
			  (otherwise nil))
	 :indent t)
     ,@body))

;;;; ### Symbols
(defmacro bound? (symbol)
  "Symbol -> Boolean
Return value of SYMBOL if bound, silencing UNBOUND-VARIABLE errors."
  `(handler-case (symbol-value ',symbol)
     (unbound-variable () nil)))


;;;; ### System
(defun quit (&optional (code 0))
  "Function to exit the Lisp implementation.

Modified from http://files.b9.com/lboot/utils.lisp"
    #+allegro (excl:exit code :quiet t)
    #+clisp (#+lisp=cl ext:quit #-lisp=cl lisp:quit code)
    #+(or cmu scl) (ext:quit code)
    #+cormanlisp (win32:exitprocess code)
    #+gcl (lisp:bye code)
    #+lispworks (lw:quit :status code)
    #+lucid (lcl:quit code)
    #+sbcl (sb-ext:exit :code (typecase code (number code) (null 0) (t 1)))
    #+mcl (ccl:quit code)
    #-(or allegro clisp cmu scl cormanlisp gcl lispworks lucid sbcl mcl)
    (error 'not-implemented :proc (list 'quit code)))
