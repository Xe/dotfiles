(setq xe-tools-packages
      '(cheatsheet
        ix
        vimish-fold
        prodigy
        (multi-term :location local)))

(defun xe-tools/init-cheatsheet ()
  (use-package cheatsheet))

(defun xe-tools/init-ix ()
  (use-package ix))

(defun xe-tools/init-vimish-fold ()
  (use-package vimish-fold))

(defun xe-tools/init-prodigy ()
  (use-package prodigy))

(defun xe-tools/init-multi-term ()
  (use-package multi-term)
  (setq multi-term-program "/bin/bash"))
