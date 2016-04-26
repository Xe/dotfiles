(setq xe-terraform-packages
      '(
        terraform-mode
        hcl-mode
        ))

(setq xe-terraform-excluded-packages '())

(defun xe-terraform/init-terraform-mode ()
  (use-package terraform-mode))

(defun xe-terraform/init-hcl-mode ()
  (use-package terraform-mode))
