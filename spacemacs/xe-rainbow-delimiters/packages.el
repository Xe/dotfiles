;;; rainbow-delimiters mode for xena's spacemacs

(setq xe-rainbow-delimiters-packages
      '(rainbow-delimiters))

(defun xe-rainbow-delimiters/init-rainbow-delimiters ()
    (use-package rainbow-delimiters)
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))
