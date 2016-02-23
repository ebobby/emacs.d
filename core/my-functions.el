;;; my-functions.el --- Misc functions
;;; Commentary:
;; Functions

;;; Code:
;; ask before exiting
(defun confirm-exit-emacs ()
  "Ask for confirmation before exiting Emacs."
  (interactive)
  (if (y-or-n-p "Are you sure you want to exit? ")
      (save-buffers-kill-emacs)))

(defun my-upgrade-all ()
  "Upgrades all packages."
  (interactive)
  (epl-refresh)
  (epl-upgrade))

(defun untabify-buffer ()
  "Remove all tabs from the buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  "Indent the buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(defun my-recompile-emacs (&optional FORCE)
  "Recompile Emacs configuration."
  (interactive)
  (byte-recompile-directory root-dir 0 FORCE))

(defun helm-ag-projectile-root (&optional ARG)
  "Search from projectile-project-root` which defaults to current directory if no project."
  (interactive)
  (helm-ag (projectile-project-root)))

(defun helm-do-ag-projectile-root (&optional ARG)
  "Search from projectile-project-root` which defaults to current directory if no project."
  (interactive)
  (helm-do-ag (projectile-project-root)))

(provide 'my-functions)

;;; my-functions.el ends here
