;;; swift mode for xena's spacemacs

(setq xe-swift-packages
      '(swift-mode))

(defun xe-swift/init-swift-mode ()
    (use-package swift-mode)
    (add-to-list 'flycheck-checkers 'swift))
