;;; elnode mode for xena's spacemacs

(setq xe-elnode-packages
      '(elnode
        (mustache :location (recipe
                             :fetcher github
                             :repo "Wilfred/mustache.el"))
        ht))

(defun xe-elnode/init-elnode ()
  (use-package elnode))

(defun xe-elnode/init-mustache ()
  (use-package mustache))

(defun xe-elnode/init-ht ()
  (use-package ht))
