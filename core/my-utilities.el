;;; my-utilities.el --- Misc functions
;;; Commentary:
;; Functions

;;; Code:

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

(provide 'my-utilities)

;;; my-utilities.el ends here
