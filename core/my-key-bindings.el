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
(global-unset-key (kbd "C-x m"))
(global-unset-key (kbd "C-x c"))
(global-unset-key (kbd "C-x TAB"))
(global-unset-key (kbd "C-c C-h"))
(global-unset-key (kbd "C-x C-r"))

(global-set-key (kbd "C-x C-c") 'confirm-exit-emacs)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "M-k") 'kill-this-buffer)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)
(global-set-key (kbd "C-c m") 'discover-my-major)
(global-set-key (kbd "C-|") 'hippie-expand)

; accented vowels with Hyper
(global-set-key (kbd "H-a") (lambda () (interactive) (insert "á")))
(global-set-key (kbd "H-e") (lambda () (interactive) (insert "é")))
(global-set-key (kbd "H-i") (lambda () (interactive) (insert "í")))
(global-set-key (kbd "H-o") (lambda () (interactive) (insert "ó")))
(global-set-key (kbd "H-u") (lambda () (interactive) (insert "ú")))
(global-set-key (kbd "H-n") (lambda () (interactive) (insert "ñ")))
(global-set-key (kbd "H-?") (lambda () (interactive) (insert "¿")))
(global-set-key (kbd "H-!") (lambda () (interactive) (insert "¡")))

(provide 'my-key-bindings)

;;; my-key-bindings.el ends here
