 (add-to-list 'load-path "~/.emacs.d/evil") ; only without ELPA/el-get
    (require 'evil)
    (evil-mode 1)
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; update agenda file after changes to org files
(defun th-org-mode-init ()
  (add-hook 'after-save-hook 'th-org-update-agenda-file t t))

(add-hook 'org-mode-hook 'th-org-mode-init)

;; that's the export function
(defun th-org-update-agenda-file (&optional force)
  (interactive)
  (save-excursion
    (save-window-excursion
      (let ((file "/tmp/org-agenda.txt"))
        (org-agenda-list)
        (org-write-agenda file)))))

;; do it once at startup
(th-org-update-agenda-file t)
