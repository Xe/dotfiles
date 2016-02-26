;;; packages.el --- xe-mustache layer packages file for Spacemacs.

(defconst xe-mustache-packages
  '(mustache-mode))

(defun xe-mustache/init-mustache-mode ()
  (use-package mustache-mode))
