;;; my-theme.el --- Theme loading and configuration
;; Copyright (C) 2010-2021 Francisco Soto
;; Author: Francisco Soto <ebobby@ebobby.org>
;; URL: https://github.com/ebobby/emacs.d
;;
;; This file is not part of GNU Emacs.
;; This file is free software.
;;; Commentary:
;;; Code:

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

(defvar ui-font "JetBrains Mono Nerd Font-13")

(when window-system
  (toggle-frame-maximized)
  (set-face-attribute 'default nil :font ui-font))

(push (lambda (frame)
        (set-frame-parameter frame 'font ui-font)
        (toggle-frame-maximized frame))
      after-make-frame-functions)

(provide 'my-theme)

;;; my-theme.el ends here
