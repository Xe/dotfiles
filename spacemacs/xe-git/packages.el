;;; git gutter for xena's spacemacs

(setq xe-git-packages
      '(git-gutter git-timemachine))

(defun xe-git/init-git-gutter ()
  (use-package git-gutter)
  (global-git-gutter-mode +1))

(defun xe-git/init-git-timemachine ()
  (use-package git-timemachine))
