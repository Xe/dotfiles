(setq xe-base-packages
      '((change-case :location local)
        (gitconfig)))

(defun xe-base/init-change-case ()
  (use-package change-case))

(defun xe-bade/init-gitconfig ()
  (eval-after-load "projectile" '(use-package gitconfig)))
