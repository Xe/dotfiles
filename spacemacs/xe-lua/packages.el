;;; lua mode for xena's spacemacs

(setq xe-lua-packages
      '((ob-lua :location local)
        (company-lua :location (recipe
                                :fetcher github
                                :repo "ptrv/company-lua"))))

(defun xe-lua/init-ob-lua ()
  (use-package ob-lua))

(defun xe-lua/init-company-lua ()
  (use-package company-lua)
  (add-to-list 'company-backends 'company-lua))
