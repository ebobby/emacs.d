;;; my-ui.el --- Theme loading and configuration  -*- lexical-binding: t; -*-
;; Copyright (C) 2010-2021 Francisco Soto
;; Author: Francisco Soto <ebobby@ebobby.org>
;; URL: https://github.com/ebobby/emacs.d
;;
;; This file is not part of GNU Emacs.
;; This file is free software.
;;; Commentary:
;;; Code:

;; Doom theme.
(use-package doom-themes
  :config
  (setq doom-dracula-brighter-comments t
        doom-dracula-brighter-modeline t
        doom-dracula-colorful-headers t
        doom-dracula-padded-modeline nil
        doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-themes-padded-modeline nil)
  (load-theme 'doom-dracula t)

  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :after (doom-themes)
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-icon t
        doom-modeline-height 35
        doom-modeline-project-detection 'projectile
        doom-modeline-buffer-file-name-style 'truncate-with-project
        doom-modeline-bar-width 1
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-state-icon t
        doom-modeline-buffer-modification-icon t
        doom-modeline-minor-modes nil
        doom-modeline-enable-word-count nil
        doom-modeline-buffer-encoding t
        doom-modeline-indent-info nil
        doom-modeline-checker-simple-format t
        doom-modeline-vcs-max-length 12
        doom-modeline-env-version t
        doom-modeline-irc-stylize 'identity
        doom-modeline-github-timer nil
        doom-modeline-gnus-timer nil)
  (with-eval-after-load "doom-modeline"
    (doom-modeline-def-modeline 'main
      '(bar workspace-name window-number modals matches follow buffer-info remote-host buffer-position word-count parrot selection-info)
      '(objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs "   "))))

;; Margins around windows.
(use-package spacious-padding
  :config
  (spacious-padding-mode 1))

;; Frame and font setup for standalone emacs.
;; macOS font and maximizing are set in early-init.el.
(when (and window-system (not (eq system-type 'darwin)))
  (set-frame-font (if (> (display-pixel-width) 2560)
                      "Monaspace Neon-14"
                    "Monaspace Neon-12")))

(provide 'my-ui)

;;; my-ui.el ends here
