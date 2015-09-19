;;; my-modes.el --- Misc modes
;;; Commentary:

;;; Code:

(add-hook 'org-mode-hook
          (lambda ()
            (writegood-mode)))

(setq-default org-tags-column 0)

(provide 'my-modes)

;;; my-modes.el ends here
