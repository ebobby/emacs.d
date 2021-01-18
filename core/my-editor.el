;;; my-editor.el --- Editor configuration
;;; Commentary:
;; Editing

;;; Code:

;; Remove whitespace on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(require 'gcmh)
(add-hook 'after-init-hook 'gcmh-mode)

;; Move blocks of text around
(require 'move-text)

;; Use aspell instead of ispell
(require 'flyspell)
(setq-default ispell-program-name "aspell" ispell-extra-args '("--sug-mode=ultra"))
(add-hook 'text-mode-hook (lambda () (flyspell-mode +1)))
(add-hook 'yaml-mode-hook (lambda () (flyspell-mode -1)))
(add-hook 'org-mode-hook (lambda ()
                           (flyspell-mode +1)
                           (auto-fill-mode +1)))
;; hi-lock
(global-hi-lock-mode 1)
(setq hi-lock-auto-select-face t)

(require 'volatile-highlights)
(volatile-highlights-mode t)

;; saveplace remembers your location in a file when saving files
(require 'saveplace)
(setq save-place-file (expand-file-name "saveplace" savefile-dir))
;; activate it for all buffers
(setq-default save-place t)

;; savehist keeps track of some history
(require 'savehist)
(setq savehist-additional-variables
      ;; search entries
      '(search ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (expand-file-name "savehist" savefile-dir))
(savehist-mode +1)

;; Recent files
(require 'recentf)
(setq recentf-save-file (expand-file-name "recentf" savefile-dir)
      recentf-max-saved-items 500
      recentf-max-menu-items 25
      recentf-auto-cleanup 60)
;; ignore magit's commit message files
(add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
(add-to-list 'recentf-exclude (expand-file-name "elpa" root-dir))
(add-to-list 'recentf-exclude (expand-file-name "ido.hist" savefile-dir))
;; Save list every 5 minutes.
(run-at-time nil (* 5 60) 'recentf-save-list)
(recentf-mode +1)

;; autofill
(require 'flycheck)
(require 'smartparens-config)

(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)
(show-smartparens-global-mode +1)

(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))

(add-hook 'prog-mode-hook (lambda ()
                            (ignore-errors
                              (imenu-add-menubar-index))
                            (flycheck-mode +1)
                            ;(flyspell-prog-mode)
                            (display-line-numbers-mode +1)
                            (smartparens-mode +1)))

(require 'expand-region)
(require 'visual-regexp)

(require 'anzu)
(global-anzu-mode)

(require 'diff-hl)
(global-diff-hl-mode +1)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)

(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

(require 'hl-line)
(global-hl-line-mode +1)

(require 'tramp)
(require 'tramp-cache)
;; keep in mind known issues with zsh - see emacs wiki
(setq tramp-default-method "ssh")
(setq tramp-persistency-file-name (expand-file-name "tramp" savefile-dir))

;; Dumb jump
(dumb-jump-mode)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

;; Get path from shell
(require 'exec-path-from-shell)
(setq exec-path-from-shell-arguments '("-li"))
(exec-path-from-shell-initialize)

;; Which key
(setq-default which-key-idle-delay 2.0)
(which-key-mode)

;; Org mode
(setq org-hide-leading-stars t)

(provide 'my-editor)

;;; my-editor.el ends here
