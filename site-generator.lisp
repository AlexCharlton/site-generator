(in-package :site-generator)
(eval-when (:execute :load-toplevel :compile-toplevel)
  (com.dvlsoft.clon:nickname-package))
;;;; # Site Generator
(export
 '(generate-site
   init-site
   include
   echo
   bound?))

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
After WALK-SITE updates the *DB*, generate the pages for every content file that needs updating. Empty directories are cleaned when done."
  (set-root-dir dir)
  (check-site)
  (print-message "Generating site...")
  (init-db)
  (walk-site *content-dir* nil nil)
  (ensure-directories-exist *site-dir*)
  (let+ ((needs-update (needs-update))
	 (configs (remove-duplicates (mapcar #'third needs-update)
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
    (iter (for config in configs)
	(gen-site (reverse config) nil)))
  (remove-empty-directories *site-dir*)
  (write-db)
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
  (unless (and (directory-exists-p *root-dir*)
	       (directory-exists-p *content-dir*)
	       (directory-exists-p *static-dir*)
	       (directory-exists-p *template-dir*)
	       (file-exists-p (merge-pathnames "config" *content-dir*))
	       (file-exists-p (merge-pathnames "index" *content-dir*)))
    (error "Not a site-generator directory: ~a" *root-dir*)))

(defun walk-site (dir configs dir-slugs)
  "Pathname ((cons Pathname Timestamp)) Plist -> nil
Recursively walk a site, tracking and parsing configs, working out slugs and calling UPDATE-DB on all non config files."
  (let* ((config (merge-pathnames "config" dir))
	 (config-contents (when (file-exists-p config)
			    (push (cons (directory-minus config *content-dir*)
					(file-write-date config))
				  configs)
			    (parse-config config)))
	 (*environment* (merge-environments config-contents *environment*))
	 (dir-slugs (add-slugs (get-dir-slugs config config-contents) dir-slugs)))
    (iter (for file in (list-directory dir))
	  (if (directory-pathname-p file)
	      (walk-site file configs dir-slugs)
	      (unless (equal (pathname-name file)
			     "config")
		(update-db file configs dir-slugs))))))

(defun update-db (file configs dir-slugs)
  "Pathname ((cons Pathname Timestamp)) Plist -> nil
Update the *DB* ENTRY of FILE when the given content file is new or has been updated, one if the file's configs have been updated, or when the file's template has been updated."
  (let* ((content (parse-page file))
	 (relative-path (directory-minus file *content-dir*))
	 (entry (gethash relative-path *db*))
	 (template (cons (get-data :template)
			 (file-write-date (merge-pathnames (get-data :template)
							   *template-dir*)))))
    (when (or (not entry)
	      (> (file-write-date file) (content-entry-last-modified entry))
	      (not (equal configs (content-entry-configs entry)))
	      (not (equal template (content-entry-template entry))))
	(setf (gethash relative-path *db*)
	      (make-content-entry
	       :needs-update t
	       :last-modified (file-write-date file)
	       :date (if entry
			 (if-let ((date (getf content :date)))
			     date
			     (content-entry-date entry))
			 (file-write-date file))
	       :tags (split-comma-or-space-separated (getf content :tags))
	       :author (getf content :author)
	       :title (getf content :title)
	       :template template
	       :configs configs
	       :pages (get-pages file (add-slugs (get-file-slugs file content)
						dir-slugs))
	       :old-pages (when entry
			    (content-entry-old-pages entry)))))))

(defun needs-update ()
  "nil -> ((Pathname Entry (Pathname)))
Return a list of all the (file entry (configs)) pairs from the *DB* that need updating."
  (iter (for (path entry) in-hashtable *db*)
	(when (content-entry-needs-update entry)
	  (collect (list path entry (mapcar #'first (content-entry-configs entry)))))))

(defun generate-page (file entry)
  "Pathname Entry -> nil
After deleting the old pages in the entry, parse the file and for each language, write the appropriate page. Update the entry to reflect this."
  (delete-old-pages entry)
  (let* ((*environment* (merge-environments
			 (parse-page (merge-pathnames file *content-dir*))
			 *environment*)))
    (iter (for lang in (getf *environment* :languages))
	  (let ((*environment* (merge-environments (list :lang lang)
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
		       :if-exists :overwrite)
    (with-open-file (in (merge-pathnames (getf *environment* :template)
					 *template-dir*)
			:direction :input)
      (with-env
	(expand in :output out)))))

(defun delete-old-pages (entry)
  "Entry -> nil
Delete the files in the list of old pages contained in ENTRY."
  (iter (for file in (content-entry-old-pages entry))
	(delete-file (merge-pathnames file *site-dir*))))

(defun remove-empty-directories (dir)
  "Pathname -> nil
Recurs depth first through a directory tree, deleting all directories that do not contain any files."
  (iter (for file in (list-directory dir))
	(when (directory-pathname-p file)
	  (remove-empty-directories file)))
  (unless (list-directory dir)
    (print-message "Removing unused directory: ~a" (directory-minus dir *site-dir*))
    (delete-directory-and-files dir)))

(defun get-file-slugs (content-file content)
  "Pathname Plist -> Plist
Return a Plist of appropriate file slugs, one for each language. The :SLUG property is given precedence for the slug name, after which :TITLE is used, and the name of the file is the fallback."
  (let ((name (pathname-name content-file)))
    (iter (for lang in (get-data :languages))
	  (collect lang)
	  (let ((slug (get-data :slug lang content))
		(title (get-data :title lang content)))
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
  (regex-replace-all "[^!#\\$&;=\\?\\-\\[\\]\\(\\)_a-zA-Z0-9~/]+"
		      (regex-replace-all "\\s+" (namestring slug) "_")
		      ""))

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
    '(:languages :default-language :use :pages-as-directories)
  "Variables that may only be defined in the top-level config file.")

(defun parse-config (config-file)
  "Pathspec -> Plist
Parse a config file using PARSE-CONTENT, throwing errors if any settings are used that shouldn't be."
  (let+ ((env (if (file-exists-p config-file)
		  (parse-content config-file))))
    (iter (for key in '(:slug :lang))
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
    (iter (for key in (append '(:directory-slug :lang) *top-level-config-vars*))
	  (when (getf env key)
	    (error "~s definition not allowed in a page content file, found in ~a"
		   key page-file)))
    env))

;;;; ## Site publishing
(defun publish-site (dir)
  (print-message "Publishing ~a" dir)
  (generate-site dir))

;;;; ## Test server
(defun run-test-server (dir &optional (port 4242))
  "Pathname &optional Integer -> nil"
  (print-message "Test server can be accessed through http://127.0.0.1:~a/" port)
  (watch-site dir))

(defun watch-site (dir)
  "Pathname -> nil
TODO check for interupts?")

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
		  (sb-ext:quit))
		 ((clon:getopt :short-name "v")
		  (format t "site-generator version ~a~%" *version*)
		  (sb-ext:quit))
		 ((clon:getopt :short-name "i")
		  (init-site dir)
		  (sb-ext:quit))
		 ((clon:getopt :short-name "p")
		  (publish-site dir)
		  (sb-ext:quit)))
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
Return true if the SYMBOL is bound, silencing UNBOUND-VARIABLE errors."
  (handler-case (boundp symbol)
    (unbound-variable () nil)))

(defmacro def-page-accessor (name entry-accessor value-var
			     (path lang &rest more-keys) &body body)
  `(progn
     (defun ,name (,path &key ,lang ,@more-keys)
       ,(if (stringp (first body))
	    (first body))
       (let* ((,lang (or ,lang (getf *environment* :lang)))
	      (entry (gethash (pathname ,path) *DB*))
	      (value (,entry-accessor entry))
	      (,value-var (or (getf value lang)
			      (getf value (getf *environment* :default-language)))))
	 (when ,value-var
	   ,@(if (stringp (first body))
		 (rest body)
		 body))))
     (export ',name)))

(def-page-accessor page-address content-entry-pages address (path lang)
  "String &keys (lang Keyword) -> String
Return the root relative address of the page denoted by PATH in the appropriate language."
  (regex-replace
   "/index.html$"
   (concatenate 'string "/"
		(namestring address))
   "/"))

(def-page-accessor page-date content-entry-date date (path lang)
  "String &keys (lang Keyword) -> String
TODO: Format date"
  date)

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
