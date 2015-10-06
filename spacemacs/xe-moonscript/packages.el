;;; moonscript mode for xena's spacemacs

(setq xe-moonscript-packages
      '((moonscript-mode :location (recipe
                                    :fetcher github
                                    :repo "k2052/moonscript-mode"))))

(defun xe-moonscript/init-moonscript-mode ()
  (use-package moonscript-mode
    :defer t))
