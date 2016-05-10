;;; hcl-format.el --- Automatic HCL formatting on save

;; Copyright 2013 The go-mode Authors. All rights reserved.
;; Use of this source code is governed by a BSD-style
;; license that can be found in the LICENSE file.

;; Author: Cadey Dodrill
;; Version: 1.0
;; Keywords: languages hcl
;; URL: https://github.com/Xe/hcl-format.el
;;
;; This file is not part of GNU Emacs.

;;; Code:

(require 'cl-lib)

(defgroup hcl-format nil
  "HCL editing tools."
  :link '(url-link "https://github.com/Xe/hcl-format.el")
  :group 'languages)

(defcustom hclfmt-command "hclfmt"
  "The 'hclfmt' command."
  :type 'string
  :group 'hcl-format)

;;;###autoload
(defun hclfmt-before-save ()
  "Add this to .emacs to run hclfmt on the current buffer when saving:
 (add-hook 'before-save-hook 'hclfmt-before-save)."

  (interactive)
  (when (eq major-mode 'go-mode) (hclfmt)))

(defun hclfmt ()
  "Format the current buffer according to the hclfmt tool."
  (interactive)
  (let ((tmpfile (make-temp-file "hclfmt" nil ".go"))
        (patchbuf (get-buffer-create "*Hclfmt patch*"))
        (errbuf (if hclfmt-show-errors (get-buffer-create "*Hclfmt Errors*")))
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8)
        our-hclfmt-args)

    (unwind-protect
        (save-restriction
          (widen)
          (if errbuf
              (with-current-buffer errbuf
                (setq buffer-read-only nil)
                (erase-buffer)))
          (with-current-buffer patchbuf
            (erase-buffer))

          (write-region nil nil tmpfile)

          (when (and (hclfmt--is-goimports-p) buffer-file-name)
            (setq our-hclfmt-args
                  (append our-hclfmt-args
                          (list "-srcdir" (file-name-directory (file-truename buffer-file-name))))))
          (setq our-hclfmt-args (append our-gofmt-args
                                       hclfmt-args
                                       (list "-w" tmpfile)))
          (message "Calling hclfmt: %s %s" gofmt-command our-gofmt-args)
          ;; We're using errbuf for the mixed stdout and stderr output. This
          ;; is not an issue because hclfmt -w does not produce any stdout
          ;; output in case of success.
          (if (zerop (apply #'call-process hclfmt-command nil errbuf nil our-gofmt-args))
              (progn
                (if (zerop (call-process-region (point-min) (point-max) "diff" nil patchbuf nil "-n" "-" tmpfile))
                    (message "Buffer is already hclfmted")
                  (hcl--apply-rcs-patch patchbuf)
                  (message "Applied hclfmt"))
                (if errbuf (hclfmt--kill-error-buffer errbuf)))
            (message "Could not apply hclfmt")
            (if errbuf (hclfmt--process-errors (buffer-file-name) tmpfile errbuf))))

      (kill-buffer patchbuf)
      (delete-file tmpfile))))


(defun hclfmt--process-errors (filename tmpfile errbuf)
  (with-current-buffer errbuf
    (if (eq hclfmt-show-errors 'echo)
        (progn
          (message "%s" (buffer-string))
          (hclfmt--kill-error-buffer errbuf))
      ;; Convert the hclfmt stderr to something understood by the compilation mode.
      (hclto-char (point-min))
      (if (save-excursion
            (save-match-data
              (search-forward "flag provided but not defined: -srcdir" nil t)))
          (insert "Your version of hclimports is too old and doesn't support vendoring. Please update goimports!\n\n"))
      (insert "hclfmt errors:\n")
      (let ((truefile
             (if (hclfmt--is-goimports-p)
                 (concat (file-name-directory filename) (file-name-nondirectory tmpfile))
               tmpfile)))
        (while (search-forward-regexp (concat "^\\(" (regexp-quote truefile) "\\):") nil t)
          (replace-match (file-name-nondirectory filename) t t nil 1)))
      (compilation-mode)
      (display-buffer errbuf))))

(defun hclfmt--kill-error-buffer (errbuf)
  (let ((win (get-buffer-window errbuf)))
    (if win
        (quit-window t win)
      (kill-buffer errbuf))))

(provide 'hcl-format)
