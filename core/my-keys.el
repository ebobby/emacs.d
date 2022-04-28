;;; my-keys.el --- Key bindings configuration.
;; Copyright (C) 2010-2021 Francisco Soto
;; Author: Francisco Soto <ebobby@ebobby.org>
;; URL: https://github.com/ebobby/emacs.d
;;
;; This file is not part of GNU Emacs.
;; This file is free software.
;;; Commentary:
;;; Code:

;; Unset a bunch of keys
(global-unset-key (kbd "C-c C-h"))
(global-unset-key (kbd "C-x 0"))
(global-unset-key (kbd "C-x 1"))
(global-unset-key (kbd "C-x 2"))
(global-unset-key (kbd "C-x 3"))
(global-unset-key (kbd "C-x C-c"))
(global-unset-key (kbd "C-x C-r"))
(global-unset-key (kbd "C-x TAB"))
(global-unset-key (kbd "C-x c"))
(global-unset-key (kbd "C-x k"))
(global-unset-key (kbd "C-x o"))
(global-unset-key (kbd "C-x o"))
(global-unset-key (kbd "s-m"))
(global-unset-key (kbd "s-n"))

;; Set basic keys
(global-set-key (kbd "<f8>") 'toggle-truncate-lines)
(global-set-key (kbd "C-\\") 'hippie-expand)
(global-set-key (kbd "C-x C-c") 'my-confirm-exit-emacs)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)
(global-set-key (kbd "M-k") 'kill-this-buffer)
(global-set-key (kbd "C-.") 'isearch-forward-symbol-at-point)

;; Remove conflicting keys from diff-mode
(add-hook 'diff-mode-hook (lambda ()
                            (local-unset-key (kbd "M-o"))
                            (local-unset-key (kbd "M-k"))))

(provide 'my-keys)

;;; my-keys.el ends here
