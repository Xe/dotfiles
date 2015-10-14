;;; moonscript mode for xena's spacemacs

(setq xe-moonscript-packages
      '((moonscript-mode :location local)))

(defun xe-moonscript/init-moonscript-mode ()
  (use-package moonscript-mode)
  (use-package moonscriptrepl-mode))
