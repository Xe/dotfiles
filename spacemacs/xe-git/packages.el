;;; git gutter for xena's spacemacs

(setq xe-git-packages
      '(git-gutter))

(defun xe-git/init-git-gutter ()
  (use-package git-gutter)
  (global-git-gutter-mode +1))
