;;; Xe extra python settings and packages

(setq xe-python-packages
      '((flycheck-mypy :location (recipe
                                  :fetcher github
                                  :repo "lbolla/emacs-flycheck-mypy"))))

(defun xe-python/init-flycheck-mypy ()
  (use-package flycheck-mypy))
