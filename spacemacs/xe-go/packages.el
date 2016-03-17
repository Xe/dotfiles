;;; go mode for xena's spacemacs

(setq xe-go-packages
      '((ob-go :location local)))

(defun xe-go/init-ob-go ()
  (use-package ob-go))
