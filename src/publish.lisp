(in-package :site-generator)
;;;; ## Site publishing
(export '(publish-site))

(defun publish-site (dir)
  "Pathspec -> nil
Generate the site, then copy its contents to the server specified by :SERVER in *ENVIRONMENT*, via Rsync."
  (generate-site dir)
  (print-message "Publishing ~a" dir)
  (let* ((config-content (parse-content (merge-pathnames "config" *content-dir*)))
         (server (getf config-content :server))
         (excludes (getf config-content :exclude))
         (command (join-strings " " "rsync"
                                "--cvs-exclude"
                                "--verbose"
                                "--archive"
                                "--compress"
                                "--progress"
                                "--copy-unsafe-links"
                                "--del"
                                (if excludes
                                    (apply #'join-strings " "
                                           (mapcar (lambda (e)
                                                     (concatenate 'string "--exclude=" e))
                                                   excludes))
                                    "")
                                (namestring *site-dir*)
                                server)))
    (unless server
      (error "No server specified in the top-level config file."))
    (print-message "~a" command)
    (asdf/interface::run-program command :output :interactive)))
