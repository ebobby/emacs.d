;;; my-utilities.el --- Misc functions
;;; Commentary:
;; Functions

;;; Code:

(defun my-upgrade-all ()
  "Upgrades all packages."
  (interactive)
  (epl-refresh)
  (epl-upgrade))

(provide 'my-utilities)

;;; my-utilities.el ends here
