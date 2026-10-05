;;; my-keys.el --- Key bindings configuration.  -*- lexical-binding: t; -*-
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
(global-unset-key (kbd "s-m"))
(global-unset-key (kbd "s-n"))

;; Set basic keys
(global-set-key (kbd "<f8>") 'toggle-truncate-lines)
(global-set-key (kbd "C-\\") 'hippie-expand)
(global-set-key (kbd "C-x C-c") 'my-confirm-exit-emacs)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)
(global-set-key (kbd "M-k") 'kill-current-buffer)
(global-set-key (kbd "C-.") 'isearch-forward-symbol-at-point)
(global-set-key (kbd "C-c g") 'writegood-mode)

;; Helm's former `C-c h' command map, mapped to Consult and built-ins.
;; Helm's s (surfraw), C-c g (Google suggest), M-g i (gid) and h h (Helm
;; manual) have no counterpart.
(defvar-keymap my-helm-command-map
  "/"       #'consult-find
  "8"       #'insert-char
  "<tab>"   #'completion-at-point
  "@"       #'package-list-packages
  "C-,"     #'quick-calc
  "C-:"     #'eval-expression
  "C-c C-x" #'async-shell-command
  "C-c SPC" #'consult-global-mark
  "C-c f"   #'consult-recent-file
  "C-x C-b" #'consult-buffer
  "C-x C-f" #'find-file
  "C-x r b" #'consult-bookmark
  "C-x r i" #'consult-register
  "I"       #'consult-imenu-multi
  "F"       #'menu-set-font
  "L"       #'find-library
  "M-g a"   #'consult-ripgrep
  "M-s o"   #'consult-line
  "M-x"     #'execute-extended-command
  "M-y"     #'consult-yank-pop
  "a"       #'helpful-symbol
  "b"       #'vertico-repeat
  "c"       #'list-colors-display
  "e"       #'xref-find-definitions
  "f"       #'consult-buffer
  "h g"     #'my-consult-info-gnus
  "h i"     #'info-lookup-symbol
  "h p"     #'finder-by-keyword
  "h r"     #'my-consult-info-emacs
  "i"       #'consult-imenu
  "l"       #'consult-locate
  "m"       #'consult-man
  "o"       #'consult-outline
  "p"       #'list-processes
  "r"       #'re-builder
  "t"       #'proced)
(keymap-global-set "C-c h" my-helm-command-map)

;; Remove conflicting keys from diff-mode
(add-hook 'diff-mode-hook (lambda ()
                            (local-unset-key (kbd "M-o"))
                            (local-unset-key (kbd "M-k"))))

(add-hook 'mhtml-mode-hook (lambda ()
                             (local-unset-key (kbd "M-o"))))
(provide 'my-keys)

;;; my-keys.el ends here
