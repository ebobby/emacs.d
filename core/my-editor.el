;;; my-editor.el --- Editor configuration  -*- lexical-binding: t; -*-
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

;; Garbage collection magic hack!
(use-package gcmh
  :hook (after-init . gcmh-mode)
  :config
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold (* 100 1024 1024)))

;; Icons (requires a Nerd Font).
(use-package nerd-icons)

;; Mise
(use-package mise
  :config
  (global-mise-mode))

;; Projectile
(use-package projectile
  :demand t
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map))
  :config
  ;; Projectile's own search commands need the rg/ag packages; use Consult's.
  (define-key projectile-mode-map [remap projectile-ripgrep] #'consult-ripgrep)
  (define-key projectile-mode-map [remap projectile-ag] #'consult-ripgrep)
  (define-key projectile-mode-map [remap projectile-grep] #'consult-grep)
  ;; Replaces `helm-projectile' (buffers, files and projects in one list).
  (define-key projectile-command-map (kbd "h") #'consult-projectile)
  (projectile-mode +1))

(use-package consult-projectile
  :defer t)

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
              ("C-," . nil)
              ("C-." . nil))
  :hook ((text-mode . flyspell-mode))
  :config
  ;; Do not spellcheck literal strings, only comments.
  (setq-default flyspell-prog-text-faces (delq 'font-lock-string-face flyspell-prog-text-faces)))

;; Syntax checking.
(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :bind (:map flycheck-mode-map
              ("C-c ! h" . consult-flycheck)))

(use-package flycheck-pos-tip
  :config
  (flycheck-pos-tip-mode))

(use-package consult-flycheck
  :defer t)

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
  :defer t
  :init
  ;; Before tramp-cache loads, or it reads the cache from the default location.
  (setq tramp-persistency-file-name (expand-file-name "tramp" savefile-dir))
  :config
  (setq tramp-default-method "ssh"))

;; Find definition.
(use-package dumb-jump
  :config
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
(use-package treemacs
  :bind (("<f9>" . treemacs-display-current-project-exclusively))
  :config
  (setq treemacs-width-is-initially-locked nil)
  (treemacs-follow-mode t))

(use-package treemacs-nerd-icons
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))

;; Multiple editing cursors.
(use-package multiple-cursors
  :bind (("C-;"     . mc/mark-all-symbols-like-this)
         ("C-c e a" . mc/edit-beginnings-of-lines)
         ("C-c e e" . mc/edit-ends-of-lines)
         ("C-c e l" . mc/edit-lines)))

;; Vertical minibuffer completion.
(use-package vertico
  :demand t
  :bind (:map vertico-map
              ("C-z" . embark-act))
  :hook (minibuffer-setup . vertico-repeat-save)
  :config
  (setq vertico-count 20
        vertico-cycle t)
  (vertico-mode))

(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map
              ("RET"   . vertico-directory-enter)
              ("DEL"   . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Match space-separated components in any order.
(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Annotations in the minibuffer.
(use-package marginalia
  :config
  ;; The default inherits `font-lock-doc-face', which doom-dracula's brighter
  ;; comments give a background, boxing the whole docstring column.
  (custom-theme-set-faces
   'user '(marginalia-documentation ((t (:inherit completions-annotations)))))
  (marginalia-mode))

(use-package nerd-icons-completion
  :after marginalia
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode))

;; Search and navigation commands.
(use-package consult
  :bind (("<f2>"    . consult-line)
         ("<f3>"    . consult-ripgrep)
         ("C-h C-r" . consult-recent-file)
         ("C-h i"   . consult-imenu)
         ("C-x b"   . consult-buffer)
         ("C-x r l" . consult-bookmark)
         ("M-g g"   . consult-goto-line)
         ("M-y"     . consult-yank-pop)
         :map minibuffer-local-map
         ("C-c C-l" . consult-history))
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (with-eval-after-load 'comint
    (keymap-set comint-mode-map "C-c C-l" #'consult-history))
  :config
  (setq consult-narrow-key "<"
        consult-project-function (lambda (_) (projectile-project-root))))

;; Act on the thing at point or the current candidate.
(use-package embark
  :bind (("C-,"   . embark-act)
         ("C-h B" . embark-bindings)))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Edit grep results (e.g. an exported `consult-ripgrep') across files.
(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t))

(use-package helpful
  :bind (("C-h v"   . helpful-variable)
         ("C-h k"   . helpful-key)
         ("C-h f"   . helpful-callable)
         ("C-h C-d" . helpful-at-point)
         ("C-h C"   . helpful-command)
         ("C-h F"   . helpful-symbol)
         ([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)))

;; Language Server Protocol
;; Set before anything can load lsp-mode (installing packages compiles files
;; that require it), as lsp-mode only reads the prefix when it's loaded.
(setq lsp-keymap-prefix "C-c l")

(defun my-lsp-completion-setup ()
  "Filter LSP completions with orderless, like every other completion."
  (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
        '(orderless)))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (lsp-completion-mode . my-lsp-completion-setup))
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]storage")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]tmp")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]log")
  (setq lsp-auto-configure t
        lsp-enable-snippet nil
        lsp-lens-enable t
        ;; Corfu shows completions; lsp-mode only provides them.
        lsp-completion-provider :none))

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

;; Replaces helm-lsp.
(use-package consult-lsp
  :after lsp-mode
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols)
  (define-key lsp-mode-map (kbd "C-c l d") #'consult-lsp-diagnostics))

;; In-buffer completion popup.
(use-package corfu
  :demand t
  :bind (("C-'" . completion-at-point)
         :map corfu-map
         ("<backtab>" . corfu-previous)
         ("<f1>"      . corfu-info-documentation)
         ("C-h"       . corfu-info-documentation)
         ("C-w"       . corfu-info-location)
         ("C-s"       . my-corfu-move-to-minibuffer)
         ("C-M-s"     . my-corfu-move-to-minibuffer))
  :config
  (add-to-list 'corfu-continue-commands #'my-corfu-move-to-minibuffer)
  (setq corfu-auto t
        corfu-auto-delay 0.1
        corfu-auto-prefix 1
        corfu-count 20
        corfu-cycle t
        corfu-popupinfo-delay '(1.0 . 0.5))
  (global-corfu-mode)
  (corfu-popupinfo-mode))

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Extra completion sources, tried after the mode's own.
(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev 90)
  (add-hook 'completion-at-point-functions #'cape-file 90))

;; Magit
(use-package magit
  :bind (("<f10>"   . magit-status)
         ("C-c m l" . magit-log)
         ("C-c m f" . magit-log-buffer-file)
         ("C-c m b" . magit-blame))
  :config
  ;; Magit funcalls the result of `hi-lock-revert-buffer-rehighlight', which
  ;; is nil when there are no hi-lock patterns, so every refresh errors.
  ;; Remove once Magit handles the nil.
  (advice-add 'hi-lock-revert-buffer-rehighlight :filter-return
              (lambda (fn) (or fn #'ignore))
              '((name . my-hi-lock-rehighlight-never-nil)))
  (setq magit-auto-revert-mode nil
        magit-define-global-key-bindings nil
        magit-last-seen-setup-instructions "1.4.0"))

(use-package rainbow-mode
  :defer t)

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
  :defer t
  :hook (org-mode . (lambda () (display-line-numbers-mode -1)))
  :config
  (setq org-hide-leading-stars t)
  (setq org-adapt-indentation t))

;; Editorconfig support
(use-package editorconfig
  :config
  (editorconfig-mode 1))

(provide 'my-editor)

;;; my-editor.el ends here
