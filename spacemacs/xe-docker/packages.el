;;; docker mode for xena's spacemacs

(setq xe-docker-packages
      '(docker
        (docker-tramp :location local)
        (docker-api :location (recipe
                               :fetcher github
                               :repo "Silex/docker-api.el"))))

(defun xe-docker/init-docker ()
  (use-package docker))

(defun xe-docker/init-docker-tramp ()
  (use-package docker-tramp))

(defun xe-docker/init-docker-api ()
  (use-package docker-api))
