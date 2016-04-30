;;; slack mode for xena's spacemacs

(setq xe-slack-packages
      '((slack)
        oauth2
        request
        circe
        alert
        emojify))

(defun xe-slack/init-slack ()
  (use-package slack))

(defun xe-slack/init-oauth2 ()
  (use-package oauth2))

(defun xe-slack/init-request ()
  (use-package request))

(defun xe-slack/init-circe ()
  (use-package circe))

(defun xe-slack/init-alert ()
  (use-package alert))

(defun xe-slack/init-emojify ()
  (use-package emojify))
