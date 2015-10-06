;; Hoon mode

(setq xe-hoon-packages
    '((hoon-mode :location local)))

(defun xe-hoon/init-hoon-mode ()
  (use-package hoon-mode))
