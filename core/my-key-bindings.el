;;; my-key-bindings.el --- Global bindings
;;; Commentary:
;; Key bindings

;;; Code:

;; Unset a bunch of keys
(global-unset-key (kbd "C-x C-c"))
(global-unset-key (kbd "C-x 1"))
(global-unset-key (kbd "C-x 2"))
(global-unset-key (kbd "C-x 3"))
(global-unset-key (kbd "C-x 0"))
(global-unset-key (kbd "C-x o"))
(global-unset-key (kbd "C-x o"))
(global-unset-key (kbd "C-x k"))
(global-unset-key (kbd "C-x c"))
(global-unset-key (kbd "C-x TAB"))
(global-unset-key (kbd "C-c C-h"))
(global-unset-key (kbd "C-x C-r"))
(global-unset-key (kbd "s-n"))

(global-set-key (kbd "C-x C-c") 'my-confirm-exit-emacs)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-k") 'kill-this-buffer)
(global-set-key (kbd "C-\\") 'hippie-expand)

(provide 'my-key-bindings)

;;; my-key-bindings.el ends here
