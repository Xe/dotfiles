(setq x-select-enable-primary t)
(setq x-select-enable-clipboard t)

(global-linum-mode 1)
(defun linum-format-func (line)
"Properly format the line number"
(let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
    (propertize (format (format " %%%dd " w) line) 'face 'linum)))

(setq linum-format 'linum-format-func)
(setq initial-buffer-choice (lambda () (get-buffer spacemacs-buffer-name)))
;(setq server-kill-new-buffers nil)

(defun my-terminal-config (&optional frame)
"Establish settings for the current terminal."
(if (not frame) ;; The initial call.
    (xterm-mouse-mode 1)
    ;; Otherwise called via after-make-frame-functions.
    (if xterm-mouse-mode
        ;; Re-initialise the mode in case of a new terminal.
        (xterm-mouse-mode 1))))

;; Evaluate both now (for non-daemon emacs) and upon frame creation
;; (for new terminals via emacsclient).
(my-terminal-config)
(add-hook 'after-make-frame-functions 'my-terminal-config)

(defun my-go-mode-hook () "Use goimports instead of go-fmt"
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet")))

(add-hook 'go-mode-hook 'my-go-mode-hook)

;; Experimental gb support for Emacs
;; This updates the GOPATH inside buffers that work on files located inside
;; of gb projects. For now this only works on Unix systems.
; Cribbed from https://github.com/zerok/emacs-golang-gb

;;; Code:
(defun zerok/setup-gb-gopath ()
  (interactive)
  (make-local-variable 'process-environment)
  (let ((srcPath (_zerok/get-gb-src-folder buffer-file-name)))
    (when srcPath
      (let* ((projectPath (string-remove-suffix "/" (file-name-directory srcPath)))
             (vendorPath (string-remove-suffix "/" (concat projectPath "/vendor")))
             (gopath (concat vendorPath ":" projectPath)))
        (message "Updating GOPATH to %s" gopath)
        (setenv "GOPATH" gopath)))))

(add-hook 'go-mode-hook 'zerok/setup-gb-gopath)

(defun _zerok/get-gb-src-folder (path)
  (let ((parent (directory-file-name (file-name-directory path)))
        (basename (file-name-nondirectory path)))
    (cond ((equal "src" basename)
           (string-remove-suffix "/" path))
          ((equal "/" parent)
           nil)
          (t
           (_zerok/get-gb-src-folder parent)))))

; Disable the highlighting for the current line
(global-hl-line-mode -1)

; I use .zsh for z shell scripts
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

;; ---------------------------------------------------------------------------
;; HELM COMPANY
;; to be used for 'fuzzy matching'
;; make sure that helm-company is in additional packages as such:
;; dotspacemacs-additional-packages '(
;;                                    helm-company
;;                                    )
;;
;; This is code that I got from tuhdo on Gitter:
(with-eval-after-load 'helm-company
  (setq helm-source-company
        (helm-build-in-buffer-source "Company"
        :data (lambda ()
                (helm-company-init)
                (helm-attr 'company-candidates))
        :fuzzy-match t
        :keymap helm-company-map
        :persistent-action 'helm-company-show-doc-buffer
        :persistent-help "Show document (If available)"
        :action helm-company-actions)
        ))

;; now running M-x and helm-company will open a buffer that one can find
;; auto-completion options with. For example, in .spacemacs running
;; helm-company and then entering 'lmanyap' will give the option of
;; 'helm-company-map' as it fuzzy searches

;; Key Binding
;; I figured I'm going to be running this while typing so just used an
;; insert mode map of Ctrl+o
(define-key evil-insert-state-map (kbd "C-o") 'helm-company)

;;; cribbed from https://github.com/TheWizardTower/dotfiles/blob/master/emacs.d/site-start.d/90_golang.el#L21
;;; Fix for https://github.com/syl20bnr/spacemacs/issues/2495.
(setq flycheck-check-syntax-automatically '(new-line save))
