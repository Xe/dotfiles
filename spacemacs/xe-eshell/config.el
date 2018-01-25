(defun eshell/clear ()
  "clear the eshell buffer."
  (eshell/clear-scrollback))

(defun eshell/e (arg)
  "opens a given file in emacs from eshell"
  (find-file arg))

(defun eshell/eh (arg)
  "opens a file in emacs from shell horizontally"
  (split-window-vertically)
  (other-window 1)
  (find-file arg))

(defun eshell/ev (arg)
  "opens a file in emacs from shell vertically"
  (split-window-horizontally)
  (other-window 1)
  (find-file arg))

(defun eshell/status ()
  "trigger a git status lookup"
  (magit-status))

(defun eshell/commit ()
  "trigger a magit-commit for all edited files"
  (magit-commit))

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
    (setq str (replace-match "" t t str)))
    str)

(defun eshell/create-branch (kind name)
  "Create a branch via my usual format"
  (let ((separator "/")
        (username (chomp (shell-command-to-string "git config github.user"))))
  (magit-branch-and-checkout (concat username separator kind separator name) "HEAD")))

