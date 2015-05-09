;;; my-modes.el --- Misc modes
;;; Commentary:

;;; Code:

(add-hook 'org-mode-hook
          (lambda ()
            (writegood-mode)))

(provide 'my-modes)

;;; my-modes.el ends here
