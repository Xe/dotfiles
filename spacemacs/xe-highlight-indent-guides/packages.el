;;; highlight-indent-guides mode for xena's spacemacs

(setq xe-highlight-indent-guides-packages
      '((highlight-indent-guides :location (recipe
                                           :fetcher github
                                           :repo "DarthFennec/highlight-indent-guides"))))

(defun xe-highlight-indent-guides/init-highlight-indent-guides ()
  (use-package highlight-indent-guides))

