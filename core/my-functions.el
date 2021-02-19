;;; my-functions.el --- Miscellaneous functions.
;; Copyright (C) 2010-2021 Francisco Soto
;; Author: Francisco Soto <ebobby@ebobby.org>
;; URL: https://github.com/ebobby/emacs.d
;;
;; This file is not part of GNU Emacs.
;; This file is free software.
;;; Commentary:
;;; Code:

(defun my-confirm-exit-emacs ()
  "Ask for confirmation before exiting Emacs."
  (interactive)
  (if (> (length (visible-frame-list)) 1)
      (delete-frame)
    (if (y-or-n-p "Are you sure you want to exit? ")
        (save-buffers-kill-emacs))))

(defun my-untabify-buffer ()
  "Remove all tabs from the buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun my-transparency (value)
  "Set the transparency of the frame window with VALUE."
  (interactive "nTransparency Value 0 - 100 opaque: ")
  (set-frame-parameter (selected-frame) 'alpha value))

(defun my-indent-buffer ()
  "Indent the buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun my-cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (my-indent-buffer)
  (my-untabify-buffer)
  (delete-trailing-whitespace))

(defun my-recompile-emacs (&optional FORCE)
  "Recompile Emacs configuration."
  (interactive)
  (byte-recompile-directory root-dir 0 FORCE))

(defun my-neotree-project ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (helm-ls-git-root-dir))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))

(defun my-reload-config ()
  "Reload configuration."
  (interactive)
  (load user-init-file))

(defun projectile-project-root (&optional dir)
  "Replacement for projectile's project root function, some packages like it.  Ignore DIR."
  (helm-ls-git-root-dir))

(defun my-helm-do-ag-project-root ()
  "Run `'ag`' on project root."
  (interactive)
  (helm-do-ag (helm-ls-git-root-dir)))

(defun my-helm-project-kill-buffers ()
  "Kill all buffers in project."
  (interactive)
  (mapcar 'kill-buffer
          (helm-browse-project-get-buffers (helm-ls-git-root-dir))))

(provide 'my-functions)

;;; my-functions.el ends here
