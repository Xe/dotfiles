(setq x-select-enable-primary t)
(setq x-select-enable-clipboard t)

(global-linum-mode 1)
(defun linum-format-func (line)
"Properly format the line number"
(let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
    (propertize (format (format " %%%dd " w) line) 'face 'linum)))

(setq linum-format 'linum-format-func)
(setq initial-buffer-choice (lambda () (get-buffer spacemacs-buffer-name)))
(setq server-kill-new-buffers nil)

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
