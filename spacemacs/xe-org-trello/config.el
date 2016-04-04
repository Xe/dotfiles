(defun sync-org-file-from-trello ()
  "Sync all remote trello tasks from the local board"
  (interactive)
  (let ((current-prefix-arg (current-buffer)))
    (call-interactively 'org-trello-sync-buffer)))
