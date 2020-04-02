;;; my-ui.el --- UI
;;; Commentary:
;; UI.

;;; Code:

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

;; Truncate lines
(setq-default truncate-lines t)

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
(require 'all-the-icons)

(require 'doom-themes)

;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

(load-theme 'doom-nord t)

(doom-themes-visual-bell-config)
(doom-themes-neotree-config)
(doom-themes-org-config)

;; this has to be run from time to time?
;(all-the-icons-install-fonts)

(require 'doom-modeline)
(setq doom-modeline-project-detection 'project)
(doom-modeline-mode 1)

;; Font
(when window-system
  ;;(toggle-frame-fullscreen)
  (my-transparency 100)
  ;;  (set-face-attribute 'default nil :font "xft:-JB  -JetBrains Mono-normal-normal-normal-*-19-*-*-*-m-0-iso10646-1")
  (set-face-attribute 'default nil :font "Source Code Pro-13"))

(provide 'my-ui)

;;; my-ui.el ends here
