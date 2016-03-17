;;; docker mode for xena's spacemacs

(setq xe-docker-packages
      '(docker
        (docker-tramp :location local)
        marcopolo))

(defun xe-docker/init-docker ()
  (use-package docker))

(defun xe-docker/init-docker-tramp ()
  (use-package docker-tramp))

(defun xe-docker/init-marcopolo ()
  (use-package marcopolo))
