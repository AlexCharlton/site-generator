(in-package :site-generator)
;;;; ## Site publishing
(export '(publish-site))

(defun publish-site (dir)
  "Pathspec -> nil
Generate the site, then copy its contents to the server specified by :SERVER in *ENVIRONMENT*, via Rsync."
  (generate-site dir)
  (print-message "Publishing ~a" dir)
  (let ((server (getf (parse-content (merge-pathnames "config" *content-dir*))
		      :server)))
    (unless server
      (error "No server specified in the top-level config file."))
    (asdf/interface::run-program  (join-strings " " "rsync"
						"--cvs-exclude"
						"--verbose"
						"--archive"
						"--compress"
						"--copy-unsafe-links"
						"--del"
						(namestring *site-dir*)
						server)
				  :output :interactive)))
