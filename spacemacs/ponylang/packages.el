;;; packages.el --- ponylang Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sean T Allen <sean@monkeysnatchbanana.com>
;; URL: https://github.com/SeanTAllen/spacemacs-ponylang-layer
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq ponylang-packages
    '(
      (ponylang-mode)
      (flycheck-pony)
      (pony-snippets)
      ))

(defun ponylang/init-ponylang-mode ()
  (use-package ponylang-mode
  :ensure t
  :config
  (progn
    (add-hook
     'ponylang-mode-hook
     (lambda ()
       (set-variable 'indent-tabs-mode nil)
       (set-variable 'tab-width 2)
       (spacemacs|define-text-object ":" "return value" ":" "=>"))))))

;;(when (configuration-layer/layer-usedp 'syntax-checking)
  (defun ponylang/init-flycheck-pony ()
    (use-package flycheck-pony))

(defun ponylang/init-pony-snippets ()
  (setq pony-snippets-dir (spacemacs//get-package-directory 'pony-snippets))

  (defun pony-snippets-initialize ()
    (let ((snip-dir (expand-file-name "snippets" pony-snippets-dir)))
      (add-to-list 'yas-snippet-dirs snip-dir t)
      (yas-load-directory snip-dir)))

  (with-eval-after-load 'yasnippet (pony-snippets-initialize)))
