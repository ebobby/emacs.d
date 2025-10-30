;;; my-editor.el --- Editor configuration
;; Copyright (C) 2010-2021 Francisco Soto
;; Author: Francisco Soto <ebobby@ebobby.org>
;; URL: https://github.com/ebobby/emacs.d
;;
;; This file is not part of GNU Emacs.
;; This file is free software.
;;; Commentary:
;;; Code:

(require 'use-package)
(require 'bind-key)
(require 'use-package-ensure)

(setq use-package-always-ensure t)

;; use-package keeps packages up to date.
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t))

;; Garbage collection magic hack!
(use-package gcmh
  :hook (after-init . gcmh-mode)
  :config
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold (* 16 1024 1024 1024)))

;; All the icons!
(use-package all-the-icons
  :config
  ;;(all-the-icons-install-fonts t)
  (setq all-the-icons-scale-factor 1))

;; Mise
(use-package mise
  :ensure t
  :hook (prog-mode . mise-mode)
  :config
  (global-mise-mode))

;; Projectile
(use-package projectile
  :ensure t
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map))
  :config
  (setq projectile-completion-system 'helm)
  (projectile-mode +1))

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
(use-package flyspell
  :bind (:map flyspell-mode-map
              ("C-;" . nil)
              ("C-." . nil))
  :hook ((text-mode . flyspell-mode))
  :config
  ;; Do not spellcheck literal strings, only comments.
  (setq-default flyspell-prog-text-faces (delq 'font-lock-string-face flyspell-prog-text-faces)))

;; Syntax checking.
(use-package flycheck
  :hook (prog-mode . flycheck-mode))

(use-package flycheck-pos-tip
  :config
  (flycheck-pos-tip-mode))

;; Smart parenthesis.
(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config)
  (setq sp-autoskip-closing-pair 'always
        sp-base-key-bindings 'paredit
        sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)
  (show-smartparens-global-mode))

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
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode . diff-hl-dir-mode))
  :config
  (global-diff-hl-mode))

;; Diff visualization.
(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

;; Cursor line visual feedback.
(use-package hl-line
  :config
  (global-hl-line-mode))

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
  (setq dumb-jump-prefer-searcher 'rg)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; Load environment from login shell.
(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-arguments '("-li"))
  (exec-path-from-shell-initialize))

;; Describe key sequences.
(use-package which-key
  :config
  (setq-default which-key-idle-delay 1.0)
  (which-key-mode))

;; Tree-like file navigation.
(use-package neotree
  :bind (("<f9>" . my-neotree-project))
  :config
  (setq neo-window-fixed-size nil))

;; Multiple editing cursors.
(use-package multiple-cursors
  :bind (("C-;"     . mc/mark-all-symbols-like-this)
         ("C-c e a" . mc/edit-beginnings-of-lines)
         ("C-c e e" . mc/edit-ends-of-lines)
         ("C-c e l" . mc/edit-lines)))

;; Show major mode keys
(use-package discover-my-major
  :bind ("C-h C-m" . discover-my-major))


;; Window navigation.
(use-package ace-window
  :bind ("M-o" . ace-window))

;; Helm <3
(use-package helm
  :init (global-set-key (kbd "C-c h") 'helm-command-prefix)
  :bind (("<f2>"      . helm-occur)
         ("<f3>"      . my-helm-do-ag-project-root)
         ("C-h C-r"   . helm-recentf)
         ("C-h F"     . helm-apropos)
         ("C-h i"     . helm-imenu)
         ("C-x C-f"   . helm-find-files)
         ("C-x C-m"   . helm-M-x)
         ("C-x b"     . helm-mini)
         ("C-x r l"   . helm-bookmarks)
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
  (setq helm-M-x-fuzzy-match                  t
        helm-apropos-fuzzy-match              t
        helm-buffers-fuzzy-matching           t
        helm-completion-in-region-fuzzy-match t
        helm-exit-idle-delay                  0
        helm-ff-file-name-history-use-recentf t
        helm-ff-fuzzy-matching                t
        helm-ff-search-library-in-sexp        t
        helm-grep-ag-command                  "rg --color=always --max-columns=1000 --smart-case --search-zip --no-heading --line-number %s -- %s %s"
        helm-grep-ag-pipe-cmd-switches        '()
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

(use-package helm-flycheck)

(use-package helm-ls-git)

(use-package helm-projectile
  :config
  (helm-projectile-on))

(use-package helpful
  :bind (("C-h v"   . helpful-variable)
         ("C-h k"   . helpful-key)
         ("C-h f"   . helpful-callable)
         ("C-h C-d" . helpful-at-point)
         ("C-h C"   . helpful-command))
  :config
  (defun describe-function (function)
    "Overwrite `describe-function' with `helpful-function'."
    (helpful-callable function))

  (defun describe-variable (variable &optional buffer frame)
    "Overwrite `describe-variable' with `helpful-variable'."
    (helpful-variable variable)))

;; Language Server Protocol
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init (setq lsp-keymap-prefix "C-c l")
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]storage")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]tmp")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]log")
  (setq lsp-auto-configure t
        lsp-enable-snippet nil
        lsp-lens-enable t
        lsp-completion-provider :capf))

(use-package dap-mode
  :bind (:map dap-mode-map
              ("<f5>" . dap-debug)
              ("<f6>" . dap-breakpoint-toggle))
  :config
  (dap-auto-configure-mode +1))

(use-package lsp-ui
  :bind (:map lsp-mode-map
              ("<f12>" . lsp-ui-doc-focus-frame))
  :config
  (setq lsp-ui-doc-alignment 'window
        lsp-ui-doc-delay 1.2
        lsp-ui-doc-header t
        lsp-ui-doc-include-signature t
        lsp-ui-doc-max-height 25
        lsp-ui-doc-max-width 150
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-show-with-cursor t
        lsp-ui-peek-list-width 50
        lsp-ui-peek-peek-height 20
        lsp-ui-peek-show-directory t
        lsp-ui-sideline-enable t))

(use-package helm-lsp
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)
  (define-key lsp-mode-map (kbd "C-c l d") #'helm-lsp-diagnostics))


;; Company
(use-package company
  :bind (("C-'" . company-complete))
  :hook (after-init .  global-company-mode)
  :config
  ;; temporary hack
  ;;(setq company-backends (delete 'company-files company-backends))
  (setq company-idle-delay 0.0
        company-minimum-prefix-length 1
        company-tooltip-align-annotations t
        company-tooltip-limit 20))

;; Company icons
(use-package company-box
  :hook (company-mode . company-box-mode))

;; Magit
(use-package magit
  :bind (("<f10>"   . magit-status)
         ("C-c m l" . magit-log)
         ("C-c m f" . magit-log-buffer-file)
         ("C-c m b" . magit-blame))
  :config
  (setq magit-auto-revert-mode nil
        magit-define-global-key-bindings nil
        magit-last-seen-setup-instructions "1.4.0"))

(use-package rainbow-mode)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-identifiers
  :hook (prog-mode . rainbow-identifiers-mode))

(use-package display-line-numbers
  :hook ((prog-mode . display-line-numbers-mode)
         (text-mode . display-line-numbers-mode))
  :config
  (setq  display-line-numbers-grow-only t
         display-line-numbers-type "relative"))

;; Org mode
(use-package org
  :hook (org-mode . (lambda () (display-line-numbers-mode -1)))
  :config
  (setq org-hide-leading-stars t)
  (setq org-adapt-indentation t))

;; Musa requirement
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(provide 'my-editor)

;;; my-editor.el ends here
