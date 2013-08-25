(in-package :site-generator)
;;;; ## Utility functions

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

(defun path-directory (path)
  "Pathspec -> Pathname
Return the directory portion of the given path."
  (make-pathname :directory (pathname-directory path)))

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

(defun join-strings (&rest strings)
    " (String) -> String
Concatenates a list of strings and puts spaces between the elements. (from Common Lisp Cookbook)"
    (format nil "~{~a~^ ~}" (remove nil strings)))

(defun trim (string)
  "String -> String
Remove whitespace at the beginning and end of a string."
  (string-trim '(#\Space #\Newline #\Tab) string))

(defun split-comma-or-space-separated (words)
  (split "\\s*,\\s+|\\s*,|\\s+" words))

(defun whitespace-char-p (char)
  "Character -> Boolean
Return true if CHAR is a whitespace character."
  (if (find char '(#\Space #\Newline #\Tab) :test #'char=)
      t))

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

