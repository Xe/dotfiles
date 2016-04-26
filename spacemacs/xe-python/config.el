(eval-after-load "flycheck-mypy"
  '(progn
     (add-hook 'python-mode-hook 'flycheck-mode)
     (add-to-list 'flycheck-disabled-checkers 'python-flake8)
     (add-to-list 'flycheck-disabled-checkers 'python-pylint)))
