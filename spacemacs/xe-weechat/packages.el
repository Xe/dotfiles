;;; weechat mode for xena's spacemacs

(setq xe-weechat-packages
      '(weechat))

(defun xe-weechat/init-weechat ()
  (use-package weechat)
  (use-package weechat-tracking)
  ;(use-package weechat-notifications)
  (use-package weechat-modules)
  (setq weechat-color-list '(unspecified "black" "dim gray" "dark red" "red"
                                         "dark green" "green" "brown"
                                         "orange" "dark blue" "blue"
                                         "dark magenta" "magenta" "dark cyan"
                                          "royal blue" "dark gray" "gray")))
