;;; my-ui.el --- UI
;;; Commentary:
;; UI.

;;; Code:

;; apply syntax highlighting to all buffers
(global-font-lock-mode t)

;; show column number in bar
(column-number-mode t)

;; highlight matching parens
(show-paren-mode t)

;; blink cursor
(blink-cursor-mode -1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Theme and fonts
(require 'all-the-icons)

(require 'doom-themes)

;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

(load-theme 'doom-dark+ t)

(doom-themes-visual-bell-config)
(doom-themes-neotree-config)
(doom-themes-org-config)

;; this has to be run from time to time?
;(all-the-icons-install-fonts)

(require 'doom-modeline)
(setq doom-modeline-project-detection 'project)
(doom-modeline-mode 1)

;; Extra theme config.
(setq doom-dracula-brighter-modeline t)
(setq doom-dracula-brighter-comments t)
(setq doom-dracula-colorful-headers t)
(setq doom-dracula-comment-bg t)

(defvar ui-font "JetBrains Mono Nerd Font-11")

(when window-system
  (toggle-frame-maximized)
  (my-transparency 100)
  (set-face-attribute 'default nil :font ui-font))

(push (lambda (frame)
        (set-frame-parameter frame 'font ui-font)
        (my-transparency 100)
        (toggle-frame-maximized frame))
      after-make-frame-functions)

(provide 'my-ui)

;;; my-ui.el ends here
