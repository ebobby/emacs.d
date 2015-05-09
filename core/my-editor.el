;;; my-editor.el --- Editor configuration
;;; Commentary:
;; Editing

;;; Code:

;; tabs and indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq require-final-newline t)

;;
(setq initial-major-mode 'org-mode)
(setq echo-keystrokes 0.1)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq auto-save-list-file-prefix temporary-file-directory)

;; Revert buffers that change externally
(global-auto-revert-mode t)

;; Hippie Expand
(setq hippie-expand-try-functions-list
      '(yas-hippie-try-expand
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; smart tab
(setq tab-always-indent 'complete)

;; encoding
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; whitespace removal
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Don't inmediately display warnings that aren't emergencies.
(setq-default warning-minimum-level :emergency)

;; don't confirm opening non-existant files/buffers
(setq confirm-nonexistent-file-or-buffer nil)

;; yes, I want to kill buffers with processes attached
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; Anwsering y/n is faster than yes/no.
(fset 'yes-or-no-p 'y-or-n-p)

;; ask before exiting
(defun confirm-exit-emacs ()
  "Ask for confirmation before exiting Emacs."
  (interactive)
  (if (y-or-n-p "Are you sure you want to exit? ")
      (save-buffers-kill-emacs)))

;; Do not ask about running processes when exiting.
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

;;; Normally disabled commands
(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

(require 'diminish)

(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; In place editing.
(require 'iedit)

;; enable some really cool extensions like C-x C-j(dired-jump)
(require 'dired-x)

(require 'dired+)
(setq diredp-hide-details-initially-flag nil
      global-dired-hide-details-mode -1
      dired-recursive-deletes 'always
      dired-recursive-copies 'always)

(add-hook 'dired-mode-hook '(lambda () (dired-hide-details-mode -1)))
(toggle-diredp-find-file-reuse-dir 1)

;; disable annoying blink-matching-paren
(setq blink-matching-paren nil)

;; Use aspell instead of ispell
(require 'flyspell)
(setq-default ispell-program-name "aspell"
              ispell-extra-args '("--sug-mode=ultra"))
(add-hook 'text-mode-hook (lambda () (flyspell-mode +1)))

;; wrap lines in a tasteful way
(global-visual-line-mode 1)
(diminish 'visual-line-mode)

;; hi-lock
(global-hi-lock-mode 1)
(setq hi-lock-auto-select-face t)

(require 'volatile-highlights)
(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)

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
      recentf-auto-cleanup 'never)
;; ignore magit's commit message files
(add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
(add-to-list 'recentf-exclude (expand-file-name "elpa" root-dir))
(add-to-list 'recentf-exclude (expand-file-name "ido.hist" savefile-dir))
(recentf-mode +1)

(require 'yasnippet)
(setq yas/prompt-functions '(yas-ido-prompt))
(yas-global-mode 1)


;; autofill
(setq-default fill-column 80)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(require 'flycheck)
(add-hook 'prog-mode-hook 'flycheck-mode)
(diminish 'flycheck-mode)

(require 'nlinum)
(add-hook 'prog-mode-hook 'nlinum-mode)

;; Nice window navigation
(require 'windmove)
(windmove-default-keybindings)

(require 'move-text)
(move-text-default-bindings)

(require 'expand-region)

(require 'anzu)
(diminish 'anzu-mode)
(global-anzu-mode)

(require 'diff-hl)
(global-diff-hl-mode +1)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)

(require 'which-func)
(which-function-mode)
(setq which-func-unknown "n/a")

(require 're-builder)
(setq reb-re-syntax 'string)

(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(require 'projectile)
(setq projectile-cache-file (expand-file-name  "projectile.cache" savefile-dir))
(setq projectile-require-project-root nil)
(projectile-global-mode t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

(require 'hl-line)
(global-hl-line-mode +1)

(require 'tramp)
;; keep in mind known issues with zsh - see emacs wiki
(setq tramp-default-method "ssh")

(provide 'my-editor)

;;; my-editor.el ends here
