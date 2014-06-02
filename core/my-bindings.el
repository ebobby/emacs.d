;;; my-bindings.el --- Global bindings
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

;; New way to get out, just to make sure we didnt screw up.
(global-set-key (kbd "C-x C-c") 'confirm-exit-emacs)

;; shell
(global-set-key (kbd "<f1>") 'shell)

;; grep
(global-set-key (kbd "<f2>") 'rgrep)

;; Hotkey for truncating lines
(global-set-key (kbd "<f7>") 'toggle-truncate-lines)

;; Magit
(global-set-key (kbd "<f10>") 'magit-status)

;; Window management
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-o") 'ace-window)

;; Kill buffers
(global-set-key (kbd "M-k") 'kill-this-buffer)

;; SMEX
(global-set-key (kbd "C-x C-m") 'smex)

;; M-x is now smex localized for major mode
(global-set-key (kbd "M-x") 'smex-major-mode-commands)

;; dash at point
(global-set-key (kbd "C-c C-p C-d") 'dash-at-point)

;; repeat
(global-set-key (kbd "C-z") 'repeat)

;; Recent files
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; Search symbol at point
(global-set-key (kbd "C-<return>") 'isearch-forward-symbol-at-point)

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Better selection
(global-set-key (kbd "C-=") 'er/expand-region)

;; Jump mode
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; Browse kill ring
(global-set-key (kbd "s-y") 'browse-kill-ring)

(provide 'my-bindings)

;;; my-bindings.el ends here
