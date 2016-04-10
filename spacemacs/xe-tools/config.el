(eval-after-load "aggressive-indent"
  '(progn
    (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
    (add-hook 'css-mode-hook #'aggressive-indent-mode)
    (add-hook 'c-mode-hook #'aggressive-indent-mode)
    (add-hook 'go-mode-hook #'aggressive-indent-mode)
    (add-hook 'python-mode-hook #'aggressive-indent-mode)))
