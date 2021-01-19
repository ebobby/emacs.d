;;; my-editor.el --- Editor configuration
;; Copyright (C) 2010-2021 Francisco Soto
;; Author: Francisco Soto <ebobby@ebobby.org>
;; URL: https://github.com/ebobby/emacs.d
;;
;; This file is not part of GNU Emacs.
;; This file is free software.
;;; Commentary:
;;; Code:

(require-packages '(use-package hydra))

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
(use-package move-text
  :config
  (defhydra hydra-move-text (global-map "C-x m")
    "Move text"
    ("p" move-text-up "up")
    ("n" move-text-down "down")))

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
(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

;; Visual feedback for regexp replace.
(use-package visual-regexp
  :bind (("C-c e t" . vr/replace)
         ("C-c e q" . vr/query-replace)))

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
(use-package neotree
  :bind (("<f9>" . my-neotree-project)))

;; Multiple editing cursors.
(use-package multiple-cursors
  :bind (("C-;"     . mc/mark-all-symbols-like-this)
         ("C-c e a" . mc/edit-beginnings-of-lines)
         ("C-c e e" . mc/edit-ends-of-lines)
         ("C-c e l" . mc/edit-lines)))

;; Show major mode keys
(use-package discover-my-major
  :bind ("C-h C-m" . discover-my-major))

;; Jump around visible text.
(use-package avy
  :bind ("C-c j" . avy-goto-word-or-subword-1))

;; Window navigation.
(use-package ace-window
  :bind ("M-o" . ace-window))

;; Helm <3
(use-package helm
  :bind-keymap
  ("C-c h" . helm-command-prefix)
  :bind
  (("<f2>"      . helm-occur)
   ("<f3>"      . my-helm-do-ag-project-root)
   ("C-c p f"   . helm-browse-project)
   ("C-c p k"   . my-helm-project-kill-buffers)
   ("C-h C-r"   . helm-recentf)
   ("C-h f"     . helm-apropos)
   ("C-x C-f"   . helm-find-files)
   ("C-x C-m"   . helm-M-x)
   ("C-x b"     . helm-buffers-list)
   ("M-x"       . helm-M-x)
   ("M-y"       . helm-show-kill-ring)
   :map helm-map
   ("<tab>" . helm-execute-persistent-action)
   ("C-i"   . helm-execute-persistent-action)
   ("C-z"   . helm-select-action)
   :map flycheck-mode-map
   ("C-c ! h" . helm-flycheck)
   :map minibuffer-local-map
   ("C-c C-l" . helm-minibuffer-history)
   :map shell-mode-map
   ("C-c C-l" . helm-comint-input-ring)
   :map comint-mode-map
   ("C-c C-l" . helm-comint-input-ring))
  :config
  (require 'helm-config)
  (require 'helm-source)
  (setq helm-M-x-fuzzy-match                  t
        helm-apropos-fuzzy-match              t
        helm-buffers-fuzzy-matching           t
        helm-completion-in-region-fuzzy-match t
        helm-exit-idle-delay                  0
        helm-ff-file-name-history-use-recentf t
        helm-ff-fuzzy-matching                t
        helm-ff-search-library-in-sexp        t
        helm-imenu-fuzzy-match                t
        helm-lisp-fuzzy-completion            t
        helm-locate-fuzzy-match               t
        helm-mode-fuzzy-match                 t
        helm-move-to-line-cycle-in-source     t
        helm-net-prefer-curl                  t
        helm-recentf-fuzzy-match              t
        helm-semantic-fuzzy-match             t
        helm-split-window-in-side-p           t)
  (helm-adaptive-mode)
  (helm-mode))

(use-package helm-ag)

(use-package helm-descbinds
  :config (helm-descbinds-mode))

(use-package helm-flycheck)

(use-package helm-ls-git)

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
