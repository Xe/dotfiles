;;; Paredit

(setq xe-paredit-packages
      '((paredit :location local)))

(defun xe-paredit/init-paredit ()
  (use-package paredit))
