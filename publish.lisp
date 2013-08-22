(in-package :site-generator)
;;;; ## Site publishing
(defun publish-site (dir)
  (print-message "Publishing ~a" dir)
  (generate-site dir))

