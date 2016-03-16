(setq xe-tools-packages
      '(cheatsheet
        ix
        aggressive-indent
        evil-nerd-commenter
        vimish-fold
        prodigy
        (multi-term :location local)))

(defun xe-tools/init-cheatsheet ()
  (use-package cheatsheet))

(defun xe-tools/init-ix ()
  (use-package ix))

(defun xe-tools/init-evil-nerd-commenter ()
  (use-package evil-nerd-commenter))

(defun xe-tools/init-vimish-fold ()
  (use-package vimish-fold))

(defun xe-tools/init-prodigy ()
  (use-package prodigy))

(defun xe-tools/init-multi-term ()
  (use-package multi-term)
  (setq multi-term-program "/bin/bash"))

(defun xe-tools/init-aggressive-indent ()
  (use-package aggressive-indent)
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode))
