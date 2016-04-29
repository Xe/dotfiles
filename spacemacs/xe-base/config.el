;; X stuff
(setq x-select-enable-primary t)
(setq x-select-enable-clipboard t)

;; Line numbers
(global-linum-mode 1)
(defun linum-format-func (line)
  "Properly format the line number"
  (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
    (propertize (format (format " %%%dd " w) line) 'face 'linum)))

(setq linum-format 'linum-format-func)
(setq initial-buffer-choice (lambda () (get-buffer spacemacs-buffer-name)))

;; Enable mouse mode in terminal
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

;; Use goimports instead of gofmt
(defun my-go-mode-hook ()
  "Use goimports instead of go-fmt"
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

;; I use .zsh for z shell scripts
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

;; Other stuff
(defun how-many-region (begin end regexp &optional interactive)
  "Print number of non-trivial matches for REGEXP in region.
Non-interactive arguments are Begin End Regexp"
  (interactive "r\nsHow many matches for (regexp): \np")
  (let ((count 0) opoint)
    (save-excursion
      (setq end (or end (point-max)))
      (goto-char (or begin (point)))
      (while (and (< (setq opoint (point)) end)
                  (re-search-forward regexp end t))
        (if (= opoint (point))
            (forward-char 1)
          (setq count (1+ count))))
      (if interactive (message "%d occurrences" count))
      count)))

(defun infer-indentation-style ()
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode
  (interactive)
  (let ((space-count (how-many-region (point-min) (point-max) "^  "))
        (tab-count (how-many-region (point-min) (point-max) "^\t")))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))

(add-hook 'find-file-hook 'infer-indentation-style)

;; Better flycheck integration
(when (display-graphic-p)
  (eval-after-load 'flycheck
    '(custom-set-variables
      '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))))

(defun kill-whitespace ()
  "Kill the whitespace between two non-whitespace characters"
  (interactive "*")
  (save-excursion
    (save-restriction
      (save-match-data
        (progn
          (re-search-backward "[^ \t\r\n]" nil t)
          (re-search-forward "[ \t\r\n]+" nil t)
          (replace-match "" nil nil))))))

;; Delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Look of disapproval
;; https://github.com/mishok13/emacsen/blob/f5c01905290ff9465f61f6e7449200f614704cf5/lib/mishok-utils.el#L7-L10
(defun look-of-disapproval ()
  "Just in case we need this"
  (interactive)
  (insert "ಠ_ಠ"))

(when (boundp 'global-company-mode)
  (global-company-mode))

(defun xe-base/nim-setup ()
  "Nim mode setups"
  (add-to-list 'company-backends
               '(company-nim :with company-nim-builtin))
  (setq nim-nimsuggest-path "~/.nimble/bin/nimsuggest"))

(add-hook 'nim-mode-hook 'xe-base/nim-setup)

(setq-default rust-enable-racer t)

(setq org-hide-emphasis-markers t)

(when (display-graphic-p)
  (let* ((variable-tuple (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                              ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                              ((x-list-fonts "Verdana")         '(:font "Verdana"))
                              ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
        (base-font-color     (face-foreground 'default nil 'default))
        (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

    (custom-theme-set-faces 'user
                            `(org-level-8 ((t (,@headline ,@variable-tuple))))
                            `(org-level-7 ((t (,@headline ,@variable-tuple))))
                            `(org-level-6 ((t (,@headline ,@variable-tuple))))
                            `(org-level-5 ((t (,@headline ,@variable-tuple))))
                            `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
                            `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
                            `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
                            `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
                            `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil)))))))

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(spacemacs/set-leader-keys "she" 'eshell-here)

(defun eshell/x ()
  (insert "exit")
  (eshell-send-input)
  (delete-window))

(defun eshell/clear ()
  "clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defmacro with-face (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))

(defun shk-eshell-prompt ()
  (let ((header-bg "#fff"))
    (concat
     (with-face (concat (eshell/pwd) " ") :background header-bg)
     (with-face (format-time-string "(%Y-%m-%d %H:%M) " (current-time)) :background header-bg :foreground "#888")
     (with-face
      (or (ignore-errors (format "(%s)" (vc-responsible-backend default-directory))) "")
      :background header-bg)
     (with-face "\n" :background header-bg)
     (with-face user-login-name :foreground "blue")
     "@"
     (with-face "localhost" :foreground "green")
     (if (= (user-uid) 0)
         (with-face " #" :foreground "red")
       " $")
     " ")))
(setq eshell-prompt-function 'shk-eshell-prompt)
(setq eshell-highlight-prompt nil)
