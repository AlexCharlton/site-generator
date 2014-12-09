(in-package :site-generator)

;;;; ## Accessors
;;;; Accessors is the term used for the functions that are called in order to access information about the pages of a site.
(defun include (template-name)
  "String -> String
Treat the string as a path to a template in *TEMPLATE-DIR* and return the contents of that file."
  (read-file-into-string (merge-pathnames template-name *template-dir*)))

(defmacro def-page-accessor (name entry-accessor value-var
			     (path lang &rest more-keys) &body body)
  "Define and export the accessor NAME used to access the value of an Entry ENTRY-ACCESSOR, from the database."
  `(progn
     (defun ,name (,path &key ,lang ,@more-keys)
       ,(if (stringp (first body))
	    (first body))
       (let* ((,path (if (eq ,path :current)
			 (get-data :current-file)
			 ,path))
	      (,lang (or ,lang (getf *environment* :lang)))
	      (entry (gethash (pathname ,path) *DB*)))
	 (unless entry
	   (error "Tried to access information about a page that doesn't exist: ~s"
		  ,path))
	 (let* ((value (,entry-accessor entry))
		(,value-var (if (listp value)
				(or (getf value lang)
				    (getf value (getf *environment*
						      :default-language)))
				value)))
	   (when ,value-var
	     ,@(if (stringp (first body))
		   (rest body)
		   body)))))
     (export ',name)))

(def-page-accessor page-address content-entry-pages address (path lang)
  "String &keys (lang Keyword) -> String
Return the root relative address of the page denoted by PATH in the appropriate language."
  (regex-replace
   "/index.html$"
   (concatenate 'string "/"
		(namestring address))
   "/"))

(def-page-accessor page-date content-entry-date date
    (path lang (format '(:long-month " " :ordinal-day ", " :year " " :hour ":" (:min 2) " " :timezone)))
  "String &keys (lang Keyword) (format (Keyword or String or (Keyword Integer &optional Character))) -> String
Return the datestring formatted as per FORMAT (see local-time)."
  (format-timestring nil date :format format))

(def-page-accessor page-title content-entry-title title (path lang)
  "String &keys (lang Keyword) -> String"
  title)

(def-page-accessor page-author content-entry-author author (path lang)
  "String &keys (lang Keyword) -> String"
  author)

(def-page-accessor page-last-modified content-entry-last-modified date
    (path lang (format '(:long-month " " :ordinal-day ", " :year " " :hour ":" (:min 2) " " :timezone)))
  "String &keys (lang Keyword) (format (Keyword or String or (Keyword Integer &optional Character)))-> String"
  (format-timestring nil (universal-to-timestamp date) :format format))

(defun other-languages (&key (ul-class "languages") (selected-class "current-langage"))
  "String &key (ul-class String) (selected-class String) -> String
Return an HTML list of links to the current page in all languages."
  (let ((page (get-data :current-file)))
    (xml
      (:ul :class ul-class
	   (loop for lang in (get-data :languages)
	      do (if (eq lang (get-data :lang))
		     (htm (:li :class selected-class (str (symbol-name lang))))
		     (htm (:li (:a :href (page-address page :lang lang)
                                   (str (symbol-name lang)))))))))))

(defun get-content (page name)
  "String Keyword -> (values X Keyword Plist)
Return the raw content of the variable NAME from PAGE. The :lang from the calling *ENVIRONMENT* is used."
  (let ((*environment* (merge-environments
			(list :lang (getf *environment* :lang)
			      :default-language (getf *environment* :default-language))
			(parse-content (merge-pathnames page *content-dir*)))))
    (get-data name)))

(defun get-sorted-pages (directory order)
  "String :ascending|:descending -> (String)
Return the list of page paths of pages in DIRECTORY from the *DB*, sorted by date in ORDER.

TODO: Currently returns all pages in DIRECTORY and its subdirectories. There should be an option to specify this behaviour"
  (let ((regex (create-scanner (format nil "^~a" (pathname-as-directory directory))))
	(time-sort (ecase order
		     (:ascending #'timestamp<)
		     (:descending #'timestamp>))))
    (remove-if (lambda (x) (scan "index$" x))
     (mapcar #'car
	     (sort (iter (for (path entry) in-hashtable *db*)
			 (when (and (scan regex (namestring path)))
			   (collect (cons (namestring path) entry))))
		   (lambda (a b) (funcall time-sort
					  (content-entry-date (cdr a))
					  (content-entry-date (cdr b)))))))))

(defun get-pages (directory &key number (start 0) (order :descending))
  "String &key (number Integer) (start Integer) (order :descending|:ascending)
Get the date sorted pages from DIRECTORY, sorted in ORDER. START specifies an offset into the full list of pages which defaults to zero (no offset). At most NUMBER pages are returned, or all remaining pages after START, if no NUMBER."
  (let ((pages (get-sorted-pages directory order)))
    (subseq pages start
	    (when (and (integerp number)
		       (<= (+ start number) (length pages)))
	      (+ start number)))))

(defun next-page (directory &optional page)
  "String String -> String OR Nil
Return the page in DIRECTORY chronologically next from PAGE."
  (let* ((pages (get-sorted-pages directory :ascending))
	 (page (or page (get-data :current-file)))
	 (position (position page pages :test #'equal)))
    (when (and position
	       (< (1+ position) (length pages)))
      (elt pages (1+ position)))))

(defun prev-page (directory &optional page)
  "String String -> String OR Nil
Return the page in DIRECTORY chronologically previous to PAGE."
  (let* ((pages (get-sorted-pages directory :ascending))
	 (page (or page (get-data :current-file)))
	 (position (position page pages :test #'equal)))
    (when (and position
	       (plusp position ))
      (elt pages (1- position)))))

(defun static-files (directory)
  "String -> (String)"
  (iter (for file in (list-directory (merge-pathnames *static-dir* directory)))
        (unless (hidden-p file)
          (collect (concatenate 'string "/"
                                (namestring (directory-minus file *root-dir*)))))))
