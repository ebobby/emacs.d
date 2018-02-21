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

(defun my-install-official-docset (docset)
  "Download dash official DOCSET unless it already exists."
  (interactive)
  (unless (helm-dash-docset-installed-p docset)
    (helm-dash-install-docset docset)))

(defun my-install-user-docset (docset)
  "Download dash user DOCSET unless it already exists."
  (interactive)
  (unless (helm-dash-docset-installed-p docset)
    (helm-dash-install-user-docset docset)))

(defmacro with-overwritten-function (f1 f2 &rest body)
  "Overwrite F1 with F2 while running BODY."
  `(letf (((symbol-function ,f1) (symbol-function ,f2)))
     ,@body))

(provide 'my-functions)

;;; my-functions.el ends here
