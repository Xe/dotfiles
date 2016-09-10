(setq xe-base-packages
      '((change-case :location local)
        (gitconfig)
        (virtual-desktops :location (recipe
                                     :fetcher github
                                     :repo "chep/virtual-desktops.el"))))

(defun xe-base/init-change-case ()
  (use-package change-case))

(defun xe-base/init-gitconfig ()
  (eval-after-load "projectile" '(use-package gitconfig)))

(defun xe-base/init-virtual-desktops ()
  (use-package virtual-desktops)
  (eval-after-load 'virtual-desktops '(progn (virtual-desktops-mode) (setq virtual-desktops-auto-update t))))
