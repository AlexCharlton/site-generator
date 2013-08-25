(in-package :site-generator)
(eval-when (:execute :load-toplevel :compile-toplevel)
  (com.dvlsoft.clon:nickname-package)
  (enable-read-macros))
(setf (html-mode) :html5)
;;;; # Site Generator
(export
 '(generate-site
   init-site
   include
   echo
   bound?
   other-languages))

(defvar *version* "0.1.0")

;;;; ## Primary interface
(defvar *root-dir*)
(defvar *content-dir*)
(defvar *site-dir*)
(defvar *template-dir*)
(defvar *static-dir*)

(defvar *DB*)
(defvar *DB-file*)

(defstruct content-entry
  "An record of a content file of the information that is used for generating the site and that should be known between other files. Stored in *DB*."
  needs-update
  last-modified
  creation-date
  date
  tags
  author
  title
  template
  configs
  pages
  old-pages)

(defvar *quiet* nil "Should messages be silenced?")

(defun print-message (format-string &rest args)
  "String &rest Things -> nil
Used to print an informative message regarding the status of site generation, respecting the variable *QUIET*."
  (unless *quiet*
    (apply #'format t (concatenate 'string format-string "~%") args)))

(defun generate-site (dir)
  "Pathname -> nil
After updating the database, generate the pages for every content file that needs updating."
  (set-root-dir dir)
  (check-site)
  (print-message "Generating site...")
  (init-db)
  (update-db)
  (update-site (needs-update))
  (print-message "Done generating site."))

(defun init-site (dir)
  "Pathspec -> nil
Initialize the directory structure of a site.
TODO?: Add keyword arguments to be inserted into config file"
  (set-root-dir dir)
  (print-message "Initializing site-generator site at ~a" dir)
  (ensure-directories-exist *root-dir*)
  (iter (for dir in (list *content-dir* *template-dir* *static-dir*))
	(ensure-directories-exist dir))
  (touch-file (merge-pathnames "config" *content-dir*))
  (touch-file (merge-pathnames "index" *content-dir*)))

(defun set-root-dir (dir)
  "Pathspec -> nil
Set the root directory of the site, and all corresponding directories."
  (setf *root-dir* (merge-pathnames (pathname-as-directory dir))
	*DB-file* (merge-pathnames ".database" *root-dir*)
	*content-dir* (merge-pathnames "content/" *root-dir*)
	*site-dir* (merge-pathnames "site/" *root-dir*)
	*template-dir* (merge-pathnames "templates/" *root-dir*)
	*static-dir* (merge-pathnames "static/" *root-dir*)))

(defun check-site ()
  "nil -> nil
Determine if a *ROOT-DIR* is a site-generator directory."
  (unless (and (directory-exists-p *root-dir*)
	       (directory-exists-p *content-dir*)
	       (directory-exists-p *static-dir*)
	       (directory-exists-p *template-dir*)
	       (file-exists-p (merge-pathnames "config" *content-dir*)))
    (error "Not a site-generator directory: ~a" *root-dir*)))

(defun update-db ()
  (setf (gethash :templates *db*) (walk-templates))
  (walk-site *content-dir* nil nil))

(defun walk-site (dir configs dir-slugs)
  "Pathname ((cons Pathname Timestamp)) Plist -> nil
Recursively walk a site, tracking and parsing configs, working out slugs and calling UPDATE-ENTRY on all non config files."
  (let* ((config (merge-pathnames "config" dir))
	 (config-contents (when (file-exists-p config)
			    (push (cons (directory-minus config *content-dir*)
					(file-write-date config))
				  configs)
			    (parse-config config)))
	 (*environment* (merge-environments config-contents *environment*))
	 (dir-slugs (add-slugs (get-dir-slugs config config-contents) dir-slugs)))
    (iter (for file in (list-directory dir))
	  (unless (hidden-p file)
	    (if (directory-pathname-p file)
		       (walk-site file configs dir-slugs)
		       (unless (equal (pathname-name file)
				      "config")
			 (update-entry file configs dir-slugs)))))))

(defun walk-templates ()
  "nil -> ({(Pathname . Integer)}+)
Return the list of template pathnames consed to the templates they depend on where a dependancy is a (path . file-write-date) pair."
  (let+ (templates
	 ((&flet get-depends (path)
	    (push (cons (cons (directory-minus path *template-dir*)
			      (file-write-date path)) nil) templates)
	    (with-open-file (s path)
	      (iter (for line = (read-line s nil 'eof))
		    (until (eq line 'eof))
		    (register-groups-bind (template)
			("\\$\\(\\s*include\\s+\"(.*)\"\\s*\\)" line :sharedp t)
		      (push (cons (pathname template)
				  (file-write-date (merge-pathnames template
								    *template-dir*)))
			    (cdr (first templates)))))))))
    (walk-directory *template-dir* #'get-depends :test (lambda (x) (not (hidden-p x))))
    (resolve-template-dependancies (reverse templates))))

(defun resolve-template-dependancies (templates)
  "({(Pathname . Integer)}+) -> ({(Pathname . Integer)}+)
Given a list of templates an their direct dependancies, return the list of templates with all dependancies."
  (let+ (((&labels resolve-template (ts)
	    (when ts
	      (cons (first ts)
		    (append (resolve-template (rest (get-template (first (first ts))
								  templates)))
			    (resolve-template (rest ts))))))))
    (iter (for template in templates)
	  (collect (cons (first template) (resolve-template (rest template)))))))

(defun get-template (path templates)
  "Pathspec ({(Pathname . Integer)}+) -> {(Pathname . Integer)}+
"
  (find (pathname path) templates :key #'caar :test #'equal))

(defun update-site (needs-update)
  "((Pathname Entry (Pathname))) -> nil
Generate each page in NEEDS-UPDATE (which are tuples as returned from NEEDS-UPDATE), removing empty directories when done."
  (let+ ((configs (remove-duplicates (mapcar #'third needs-update)
				     :test #'equal))
	 ((&labels gen-site (configs configs-parsed)
	    "Recursively work through each config files in CONFIGS, applying that config to *ENVIRONMENT* and generating any page in NEEDS-UPDATE that matches the configs parsed so far. NEEDS-UPDATE is updated to reflect the files that still need updating."
	    (let ((*environment* (merge-environments
				  (parse-config (merge-pathnames (first configs)
								 *content-dir*))
				  *environment*)))
	      (push (first configs) configs-parsed)
	      (setf needs-update
		    (iter (for update in needs-update)
			  (let+ (((path entry confs) update))
			    (if (equal confs configs-parsed)
				(generate-page path entry)
				(collect update)))))
	      (when (rest configs)
		(gen-site (rest configs) configs-parsed))))))
    (ensure-directories-exist *site-dir*)
    (unless (directory-exists-p (merge-pathnames "static/" *site-dir*))
      (osicat:make-link (merge-pathnames "static" *site-dir*)
			:target *static-dir*))
    (iter (for config in configs)
	  (gen-site (reverse config) nil))
    (remove-empty-directories *site-dir*)
    (write-db)))

(defun update-entry (file configs dir-slugs)
  "Pathname ((cons Pathname Timestamp)) Plist -> nil
Update the *DB* ENTRY of FILE when the given content file is new or has been updated, one if the file's configs have been updated, or when the file's template has been updated."
  (let* ((*environment* (merge-environments (parse-page file)
					    *environment*))
	 (relative-path (directory-minus file *content-dir*))
	 (entry (gethash relative-path *db*))
	 (template (get-template (get-data :template)
				 (gethash :templates *db*))))
    (when (or (not entry)
	      (> (file-write-date file) (content-entry-last-modified entry))
	      (not (equal configs (content-entry-configs entry)))
	      (not (equal template (content-entry-template entry))))
      (let ((creation-date (if entry
			       (content-entry-creation-date entry)
			       (universal-to-timestamp (file-write-date file)))))
	(setf (gethash relative-path *db*)
	      (make-content-entry
	       :needs-update t
	       :last-modified (file-write-date file)
	       :creation-date creation-date
	       :date (if-let ((date (get-data :date)))
		       (make-time date creation-date)
		       creation-date)
	       :tags (iter (for (lang tags) on (getf *environment* :tags) by #'cddr)
			   (collect lang)
			   (collect (split-comma-or-space-separated (first tags))))
	       :author (getf *environment* :author)
	       :title (getf *environment* :title)
	       :template template
	       :configs configs
	       :pages (get-pages file (add-slugs (get-file-slugs file) dir-slugs))
	       :old-pages (when entry
			    (content-entry-old-pages entry))))))))

(defun needs-update ()
  "nil -> ((Pathname Entry (Pathname)))
Return a list of all the (file entry (configs)) pairs from the *DB* that need updating."
  (iter (for (path entry) in-hashtable *db*)
	(when (and (eq (type-of entry) 'content-entry)
		   (content-entry-needs-update entry))
	  (collect (list path entry (mapcar #'first (content-entry-configs entry)))))))

(defun generate-page (file entry)
  "Pathname Entry -> nil
After deleting the old pages in the entry, parse the file and for each language, write the appropriate page. Update the entry to reflect this."
  (delete-old-pages entry)
  (let* ((*environment* (merge-environments
			 (parse-page (merge-pathnames file *content-dir*))
			 *environment*)))
    (iter (for lang in (getf *environment* :languages))
	  (let ((*environment* (merge-environments (list :lang lang
							 :current-file (namestring file))
						   *environment*)))
	    (write-page (merge-pathnames (getf (content-entry-pages entry) lang)
					 *site-dir*))))
    (setf (content-entry-needs-update entry) nil
	  (content-entry-old-pages entry) (content-entry-pages entry))))

(defun write-page (page)
  "Pathspec -> nil
For the file PAGE, write the expansion of the current template with the current environment found in *ENVIRONMENT*."
  (unless (getf *environment* :template)
    (error "Template not specified for page: ~a" page))
  (print-message "Writing page ~a" (directory-minus page *site-dir*))
  (ensure-directories-exist page)
  (with-open-file (out page
		       :direction :output
		       :if-does-not-exist :create
		       :if-exists :supersede)
    (with-open-file (in (merge-pathnames (getf *environment* :template)
					 *template-dir*)
			:direction :input)
      (with-env
	(expand in out)))))

(defun delete-old-pages (entry)
  "Entry -> nil
Delete the files in the list of old pages contained in ENTRY."
  (iter (for (lang file) on (content-entry-old-pages entry) by #'cddr)
	(when-let ((file (file-exists-p (merge-pathnames file *site-dir*))))
	  (delete-file (merge-pathnames file *site-dir*)))))

(defun remove-empty-directories (dir)
  "Pathname -> nil
Recurs depth first through a directory tree, deleting all directories that do not contain any files."
  (iter (for file in (list-directory dir :follow-symlinks nil))
	(when (directory-pathname-p file)
	  (remove-empty-directories file)))
  (unless (or (directory-pathname-p dir)
	      (list-directory dir))
    (print-message "Removing unused directory: ~a" (directory-minus dir *site-dir*))
    (delete-directory-and-files dir)))

(defun get-file-slugs (content-file)
  "Pathname -> Plist
Return a Plist of appropriate file slugs, one for each language. The :SLUG property is given precedence for the slug name, after which :TITLE is used, and the name of the file is the fallback."
  (let ((name (pathname-name content-file)))
    (iter (for lang in (get-data :languages))
	  (collect lang)
	  (let ((slug (get-data :slug lang))
		(title (get-data :title lang)))
	    (collect (slugify (cond
				((and slug (string/= slug ""))
				 slug)
				((and title (string/= title ""))
				 title)
				(t name))))))))

(defun get-dir-slugs (config-file env)
  "Pathname Plist -> Plist
Return a Plist of appropriate directory slugs, one for each language. The :DIRECTORY-SLUG property is given priority, and the name of the directory in which the config file resides is fallen back upon for that directory slug. If this is the top level directory, the name of the language is used for all but the default language."
  (let+ ((relative-dir (directory-minus (path-directory config-file)
				       *content-dir*)))
    (iter (for lang in (or (getf env :languages)
			   (getf *environment* :languages)))
	  (collect lang)
	  (collect (slugify
		    (pathname-as-directory
		     (or (when (pathname-equal relative-dir #p"")
			   (if (eq lang (or (getf env :default-language)
					    (getf *environment* :default-language)))
			       ""
			       (format nil "~(~a~)" lang)))
			 (first (getf (getf env :directory-slug) lang))
			 (parent-directory relative-dir))))))))

(defun get-pages (file slugs)
  "Pathname Plist -> Plist
Return a plist of the relative paths of the pages for FILE, given its SLUGS. This depends upon :PAGES-AS-DIRECTORIES."
  (iter (for lang in (getf *environment* :languages))
	(collect lang)
	(collect (concatenate 'string
			   (namestring (getf slugs lang))
			   (when (and (getf *environment* :pages-as-directories)
				      (string/= (pathname-name file) "index"))
			     "/index")
			   ".html"))))

(defun slugify (slug)
  "Pathspec -> String
Given a pathspec, return a string that is a valid URI."
  (let ((replace-list '(("\\s+" . "_")
			("[‘’]" "'")
			("[“”]" "\"")
			("—" "-")
			("[ÀÁÂÃÄÅ]" "A")
			("Æ" "AE")
			("Ç" "C")
			("[ÈÉÊË]" "E")
			("[ÌÍÎÏ]" "I")
			("Ñ" "N")
			("[ÒÓÔÖÕ]" "O")
			("Œ" "OE")
			("[ÙÚÛÜ]" "U")
			("[ŸÝ]" "Y")
			("[àáâãäå]" "a")
			("æ" "ae")
			("ç" "c")
			("[èéêë]" "e")
			("[ìíîï]" "i")
			("ñ" "n")
			("[òóôöõ]" "o")
			("œ" "oe")
			("[ùúûü]" "u")
			("[ÿý]" "y")
			("[^!#\\$&;=\\?\\-\\[\\]\\(\\)_a-zA-Z0-9~/]+". ""))))
    (iter (for (regex . replace) in replace-list)
	  (for str initially (namestring slug) then new)
	  (for new = (regex-replace-all regex str replace))
	  (finally (return new)))))

(defun add-slugs (slug-new slug-old)
  "Plist Plist -> Plist
Combines two slugs so that SLUG-NEW is in the directory of SLUG-OLD
Assumes NEW-SLUG has slugs for every language, and OLD-SLUG, if not empty, also has slugs for every language."
  (iter (for (lang new-slug) on slug-new by #'cddr)
	(collect lang)
	(collect (if-let ((old-slug (getf slug-old lang)))
		   (merge-pathnames new-slug old-slug) 
		   new-slug))))

(defun init-db ()
  "nil -> nil
Read the file *DB-FILE* and set *DB* to be its Plist contents."
  (setf *db* (plist-hash-table
	      (when (file-exists-p *db-file*)
		(with-open-file (s *db-file*)
		  (read s)))
	      :test #'equal)))

(defun write-db ()
  "nil -> nil
Print the contents of *DB* into *DB-FILE*, as a Plist."
  (with-open-file (s *db-file*
		     :direction :output
		     :if-exists :overwrite
		     :if-does-not-exist :create)
    (prin1 (hash-table-plist *db*) s)))

(defvar *top-level-config-vars*
    '(:languages :default-language :use :pages-as-directories :server)
  "Variables that may only be defined in the top-level config file.")

(defun parse-config (config-file)
  "Pathspec -> Plist
Parse a config file using PARSE-CONTENT, throwing errors if any settings are used that shouldn't be."
  (let+ ((env (if (file-exists-p config-file)
		  (parse-content config-file))))
    (iter (for key in '(:slug :lang :current-file :date))
	  (when (getf env key)
	    (error "~s definition not allowed in a config file, found in ~a"
		   key config-file)))
    (unless (equal (directory-minus (path-directory config-file)
				  *content-dir*)
		 #p"")
      (iter (for key in *top-level-config-vars*)
		   (when (getf env key)
		     (error "~s definition not allowed in any but the top-level config file, found in: ~a" key config-file))))
    env))

(defun parse-page (page-file)
  "Pathspec -> Plist
Parse a page content file using PARSE-CONTENT and throw errors if any settings are used that shouldn't be."
  (let ((env (parse-content page-file)))
    (iter (for key in (append '(:directory-slug :lang :current-file)
			      *top-level-config-vars*))
	  (when (getf env key)
	    (error "~s definition not allowed in a page content file, found in ~a"
		   key page-file)))
    env))

;;;; ## Command line interface
(clon:defsynopsis (:postfix "DIRECTORY")
  (text :contents "site-generator is a static site generator. When called with no arguments, site-generator will generate the site-generator site that resides at DIRECTORY.")
  (flag :short-name "i" :long-name "init"
	:description "Initialize a site-generator directory.")
  (flag :short-name "p" :long-name "publish"
	:description "Generate the site and publish it to the server specified in the top-level config file.")
  (lispobj :short-name "s" :long-name "test-server"
	   :argument-type :optional
	   :description "Lanch a test server for a site-generator site, updating the pages when files are changed on disk. Optionally accepts a value for the port on which the server listens."
	   :fallback-value 4242
	   :argument-name "PORT")
  (flag :short-name "q" :long-name "quiet"
	:description "Silence output.")
  (flag :short-name "h" :long-name "help"
	:description "Print this help and exit.")
  (flag :short-name "v" :long-name "version"
	:description "Print version number and exit."))

(defun main (argv)
  "String -> nil
Entry point. Perform the relevant action based on the command line options."
  (declare (ignore argv))
  (in-package :site-generator)
  (handler-case
      (progn (clon:make-context)
	     (when (clon:getopt :short-name "q")
	       (setf *quiet* t))
	     (let ((dir (get-site-dir)))
	       (cond
		 ((clon:getopt :short-name "h")
		  (clon:help)
		  (sb-ext:exit))
		 ((clon:getopt :short-name "v")
		  (format t "site-generator version ~a~%" *version*)
		  (sb-ext:exit))
		 ((clon:getopt :short-name "i")
		  (init-site dir)
		  (sb-ext:exit))
		 ((clon:getopt :short-name "p")
		  (publish-site dir)
		  (sb-ext:exit)))
	       (if-let ((port (clon:getopt :short-name "s")))
		 (run-test-server dir port) 
		 (generate-site dir))))
    (error (e) (format t "Error: ~a~%" e))))

(defun get-site-dir ()
  "nil -> Pathname
Return the path refereed to by the remainder of the command line options. If no path is specified, default to *DEFAULT-PATHNAME-DEFAULTS*."
  (if-let ((remainder (clon:remainder)))
    (canonical-pathname
     (let ((dir (first remainder)))
       (if (pathname-root-p dir)
	dir
	(merge-pathnames dir))))
    *default-pathname-defaults*))

;;;; ## Accessors
;;;; Accessors is the loose term for the functions that are called from templates in order to access information about the pages of a site.
(defun include (template-name)
  "String -> String
Treat the string as a path to a template in *TEMPLATE-DIR* and return the contents of that file."
  (with-output-to-string (out)
    (with-open-file (in (merge-pathnames template-name *template-dir*))
      (iter (for line = (read-line in nil 'eof))
	    (until (eq line 'eof))
	    (write-line line out)))))

(defun echo (&rest strings)
  "&rest Strings -> String
Concatenate the strings"
  (apply #'concatenate 'string strings))

(defmacro bound? (symbol)
  "Symbol -> Boolean
Return value of SYMBOL if bound, silencing UNBOUND-VARIABLE errors."
  `(handler-case (symbol-value ',symbol)
     (unbound-variable () nil)))

(defmacro def-page-accessor (name entry-accessor value-var
			     (path lang &rest more-keys) &body body)
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
  "String &keys (lang Keyword) (format (Keyword or String or (Keyword Integer &optional Character))) -> String"
  (format-timestring nil date :format format))

(def-page-accessor page-tags content-entry-tags tags (path lang)
  "String &keys (lang Keyword) -> (String)
TODO: Create formatted string of links."
  tags)

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
Return an html list of links to the current page in all languagse"
  (let ((page (get-data :current-file)))
    (html
      (:ul :class ul-class
	   (loop for lang in (get-data :languages)
	      do (if (eq lang (get-data :lang))
		     (htm (:li :class selected-class (str lang)))
		     (htm (:li (:a :href (page-address page :lang lang)
				      (str lang))))))))))
