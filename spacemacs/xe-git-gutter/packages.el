;;; git gutter for xena's spacemacs

(setq xe-git-gutter-packages
      '(git-gutter-fringe))

(defun xe-git-gutter/init-git-gutter-fringe ()
  (use-package git-gutter-fringe)
  (setq git-gutter-fr:side 'right-fringe))
