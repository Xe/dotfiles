;;; lua mode for xena's spacemacs

(setq xe-web-scale-packages
      '((vue-mode :location (recipe
                             :fetcher github
                             :repo "codefalling/vue-mode"))))

(defun xe-web-scale/init-vue-mode ()
  (use-package vue-mode))
