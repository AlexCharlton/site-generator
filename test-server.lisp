(in-package :site-generator)
;;;; ## Test server
(defvar *quit* nil)
(defvar *acceptor*)

(defun run-test-server (dir &optional (port 4242))
  "Pathname &optional Integer -> nil"
  (set-root-dir dir)
  (check-site)
  (init-db)
  (start-server port)
  (setf *quit* nil)
  (let ((site-thread (make-thread #'watch-site)))
    (iter (for line = (read-line *standard-input* nil 'eof))
	  (until (find line '(eof "quit" "exit") :test #'equalp)))
    (setf *quit* t)
    (join-thread site-thread))
  (print-message "Test server shutting down.")
  (hunchentoot:stop *acceptor*))

(defun watch-site ()
  "nil -> nil"
  (print-message "Watching site for changes...")
  (iter (until *quit*)
	(update-db)
	(when-let ((needs-update (needs-update)))
	  (update-site needs-update))
	(sleep 1)))

(defclass acceptor (hunchentoot:acceptor)
  ()
  (:default-initargs :address "127.0.0.1"))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor acceptor) request)
  (let* ((uri (subseq (hunchentoot:script-name request) 1))
	(file (merge-pathnames uri *site-dir*)))
    (when (file-exists-p file)
      (if (eq (osicat:file-kind file) :directory)
	  (hunchentoot:handle-static-file (merge-pathnames "index.html"
							   (pathname-as-directory file)))
	  (hunchentoot:handle-static-file file)))))

(defun start-server (port)
  (print-message "Starting test server.")
  (setf *acceptor* (make-instance 'acceptor :port port))
  (hunchentoot:start *acceptor*)
  (print-message "Test server can be accessed through http://127.0.0.1:~a/" port))
