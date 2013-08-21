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

(defun join-string-list (string-list)
    " (String) -> String
Concatenates a list of strings and puts spaces between the elements. (from Common Lisp Cookbook)"
    (format nil "~{~a~^ ~}" (remove nil string-list)))

(defun trim-whitespace (string)
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
