(in-package :site-generator)
;;;; ## Test server
(defun run-test-server (dir &optional (port 4242))
  "Pathname &optional Integer -> nil"
  (print-message "Starting test server.")
  (print-message "Test server can be accessed through http://127.0.0.1:~a/" port)
  (watch-site dir)
  (print-message "Test server shutting down."))

(defun watch-site (dir)
  "Pathname -> nil"
  (iter (for line = (read-line *standard-input* nil 'eof))
	(until (find line '(eof "quit" "exit") :test #'equalp))))
