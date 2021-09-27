;;; my-theme.el --- Theme loading and configuration
;; Copyright (C) 2010-2021 Francisco Soto
;; Author: Francisco Soto <ebobby@ebobby.org>
;; URL: https://github.com/ebobby/emacs.d
;;
;; This file is not part of GNU Emacs.
;; This file is free software.
;;; Commentary:
;;; Code:

(defvar ui-font "JetBrains Mono Nerd Font-13")

;; All the icons!
(use-package all-the-icons
  :config
  ;;(all-the-icons-install-fonts t)
  )

;; Doom theme.
(use-package doom-themes
  :config
  (load-theme 'doom-dracula t)
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (setq doom-themes-neotree-file-icons t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :after (doom-themes)
  :hook (after-init . doom-modeline-mode)
  :config
  ;;(setq doom-modeline-height 50)
  (setq doom-modeline-icon t)
  (setq doom-modeline-project-detection 'projectile))

;; Frame and font setup for standalone emacs.
(when window-system
  (toggle-frame-maximized)
  (set-face-attribute 'default nil :font ui-font))

;; Frame and font setup for emacsclient.
(push (lambda (frame)
        (set-frame-parameter frame 'font ui-font)
        (toggle-frame-maximized frame))
      after-make-frame-functions)

(provide 'my-theme)

;;; my-theme.el ends here
