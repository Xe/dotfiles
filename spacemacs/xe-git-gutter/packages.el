;;; git gutter for xena's spacemacs

(setq xe-git-gutter-packages
      '(git-gutter))

(defun xe-git-gutter/init-git-gutter ()
  (use-package git-gutter)
  (global-git-gutter-mode +1))
