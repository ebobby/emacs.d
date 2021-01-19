;;; my-editor.el --- Editor configuration
;; Copyright (C) 2010-2021 Francisco Soto
;; Author: Francisco Soto <ebobby@ebobby.org>
;; URL: https://github.com/ebobby/emacs.d
;;
;; This file is not part of GNU Emacs.
;; This file is free software.
;;; Commentary:
;;; Code:

(require-package 'use-package)

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; use-package keeps packages up to date.
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; Garbage collection magic hack!
(use-package gcmh
  :config
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold (* 16 1024 1024))
  (add-hook 'after-init-hook 'gcmh-mode))

;; Move blocks of text around
(use-package move-text)

(use-package hi-lock
  :config
  (setq hi-lock-auto-select-face t)
  (global-hi-lock-mode))

;; Highlight changes (for undo, yank, etc).
(use-package volatile-highlights
  :config (volatile-highlights-mode t))

;; Remember location on buffers.
(use-package saveplace
  :config
  (setq save-place-file (expand-file-name "saveplace" savefile-dir))
  (setq-default save-place t)
  (save-place-mode))

;; Keep track of history for several commands.
(use-package savehist
  :config
  (setq savehist-additional-variables '(search ring regexp-search-ring)
        savehist-autosave-interval 60
        savehist-file (expand-file-name "savehist" savefile-dir))
  (savehist-mode))

;; Recent files.
(use-package recentf
  :config
  (setq recentf-auto-cleanup 60
        recentf-max-menu-items 25
        recentf-max-saved-items 500
        recentf-save-file (expand-file-name "recentf" savefile-dir))
  ;; ignore magit's commit message files
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
  (add-to-list 'recentf-exclude (expand-file-name "elpa" root-dir))
  (add-to-list 'recentf-exclude (expand-file-name "ido.hist" savefile-dir))
  ;; Save list every 5 minutes.
  (run-at-time nil (* 5 60) 'recentf-save-list)
  (recentf-mode))

;; Syntax checking.
(use-package flycheck)
(use-package flycheck-pos-tip
  :after (flycheck)
  :config (flycheck-pos-tip-mode))

;; Smart parenthesis.
(use-package smartparens
  :config
  (require 'smartparens-config)
  (setq sp-autoskip-closing-pair 'always
        sp-base-key-bindings 'paredit
        sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)
  (show-smartparens-global-mode))

;; Smart regions.
(use-package expand-region)

;; Visual feedback for regexp replace.
(use-package visual-regexp)

;; Visual feedback for searching.
(use-package anzu
  :config
  (global-anzu-mode))

;; Version control visual feedback.
(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode))

;; Diff visualization.
(use-package ediff
  :config (setq ediff-window-setup-function 'ediff-setup-windows-plain))

;; Cursor line visual feedback.
(use-package hl-line
  :config (global-hl-line-mode))

;; Tramp configuration
(use-package tramp
  :config
  (require 'tramp-cache)
  (setq tramp-default-method "ssh"
        tramp-persistency-file-name (expand-file-name "tramp" savefile-dir)))

;; Find definition.
(use-package dumb-jump
  :config
  (setq dumb-jump-selector 'helm)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; Load environment from login shell.
(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-arguments '("-li"))
  (exec-path-from-shell-initialize))

;; Describe key sequences.
(use-package which-key
  :config
  (setq-default which-key-idle-delay 2.0)
  (which-key-mode))

;; Tree-like file navigation.
(use-package neotree)

;; Multiple editing cursors.
(use-package multiple-cursors)

;; Show major mode keys
(use-package discover-my-major)

;; Jump around visible text.
(use-package avy)

;; Window navigation.
(use-package ace-window)

;; Org mode
(setq org-hide-leading-stars t)

;; Misc configuration
(add-hook 'text-mode-hook (lambda () (flyspell-mode +1)))
(add-hook 'yaml-mode-hook (lambda () (flyspell-mode -1)))
(add-hook 'org-mode-hook (lambda ()
                           (flyspell-mode +1)
                           (auto-fill-mode +1)))

(add-hook 'prog-mode-hook (lambda ()
                            (ignore-errors
                              (imenu-add-menubar-index))
                            (flycheck-mode +1)
                            (flyspell-prog-mode)
                            (display-line-numbers-mode +1)
                            (smartparens-mode +1)))
(provide 'my-editor)

;;; my-editor.el ends here
