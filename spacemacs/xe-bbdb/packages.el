(setq xe-bbdb-packages
      '(bbdb
        bbdb-
        bbdb2erc
        bbdb-vcard
        bbdb-ext))

(defun xe-bbdb/init-bbdb ()
   (use-package bbdb))

(defun xe-bbdb/init-bbdb- ()
  (use-package bbdb-))

(defun xe-bbdb/init-bbdb2erc ()
  (use-package bbdb2erc))

(defun xe-bbdb/init-bbdb-vcard ()
  (use-package bbdb-vcard))

(defun xe-bbdb/init-bbdb-ext ()
  (use-package bbdb-ext))
