;;; my-ui.el --- UI
;;; Commentary:
;; UI.

;;; Code:

;; Truncate lines
(setq-default truncate-lines t)

;; apply syntax highlighting to all buffers
(global-font-lock-mode t)

;; No splash screen
(setq inhibit-startup-message t)

;; show column number in bar
(column-number-mode t)

;; show marks as selections
(setq transient-mark-mode t)

;; highlight matching parens
(show-paren-mode t)

;; blink cursor
(blink-cursor-mode -1)

;; force new frames into existing window
(setq ns-pop-up-frames nil)

;; no bell
(setq ring-bell-function 'ignore)

;; highlight incremental search
(defconst search-highlight t)
(defconst query-replace-highlight t)

;; Nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(fset 'yes-or-no-p 'y-or-n-p)

;; Turn off mouse interface early in startup to avoid momentary display
;; You really don't need these; trust me.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;; Theme and fonts
(require 'powerline)
(require 'moe-theme)

(setq moe-theme-resize-markdown-title '(1.5 1.4 1.3 1.2 1.0 1.0))
(setq moe-theme-resize-org-title '(1.5 1.4 1.3 1.2 1.1 1.0 1.0 1.0 1.0))
(setq moe-theme-resize-rst-title '(1.5 1.4 1.3 1.2 1.1 1.0))
(moe-theme-set-color 'magenta)

(moe-light)
(powerline-moe-theme)

(setq powerline-display-buffer-size nil)

;; Font
(when window-system
  (set-face-attribute 'default nil :font "Hack-12")
  (toggle-frame-maximized))

(provide 'my-ui)

;;; my-ui.el ends here
