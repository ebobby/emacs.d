;;; my-theme.el --- Theme loading and configuration
;; Copyright (C) 2010-2021 Francisco Soto
;; Author: Francisco Soto <ebobby@ebobby.org>
;; URL: https://github.com/ebobby/emacs.d
;;
;; This file is not part of GNU Emacs.
;; This file is free software.
;;; Commentary:
;;; Code:

(defvar ui-font "Consolas-13")

;; All the icons!
(use-package all-the-icons
  :config
  ;;(all-the-icons-install-fonts t)
  (setq all-the-icons-scale-factor 1))

;; Doom theme.
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-themes-neotree-file-icons t
        doom-molokai-brighter-comments t
        doom-molokai-brighter-modeline t)

  (load-theme 'doom-molokai t)

  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :after (doom-themes)
  :hook (after-init . doom-modeline-mode)
  :config
  ;;(setq doom-modeline-height 50)
  (setq doom-modeline-icon t)
  (setq doom-modeline-project-detection 'projectile)
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project))

;; Frame and font setup for standalone emacs.
(when window-system
  (set-frame-parameter nil 'fullscreen 'maximized)
  (set-face-attribute 'default nil :font ui-font))

(provide 'my-theme)

;;; my-theme.el ends here
