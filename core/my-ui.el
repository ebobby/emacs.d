;;; my-ui.el --- Theme loading and configuration
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
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-themes-neotree-file-icons t)

  (load-theme 'doom-monokai-pro t)

  ;; doom-monokai-pro helm-files directories are white for some reason.
  (custom-set-faces
   '(helm-ff-directory ((t (:extend t :foreground "#FFD866")))))

  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :after (doom-themes)
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-icon t)
  (setq doom-modeline-height 82)
  (setq doom-modeline-project-detection 'projectile)
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project)
  (setq doom-modeline-bar-width 1)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-buffer-modification-icon t)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-enable-word-count nil)
  (setq doom-modeline-buffer-encoding t)
  (setq doom-modeline-indent-info nil)
  (setq doom-modeline-checker-simple-format t)
  (setq doom-modeline-vcs-max-length 12)
  (setq doom-modeline-env-version t)
  (setq doom-modeline-irc-stylize 'identity)
  (setq doom-modeline-github-timer nil)
  (setq doom-modeline-gnus-timer nil)

  (with-eval-after-load "doom-modeline"
    (doom-modeline-def-modeline 'main
      '(bar workspace-name window-number modals matches follow buffer-info remote-host buffer-position word-count parrot selection-info)
      '(objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs checker "   "))))

;; Frame and font setup for standalone emacs.
(when window-system
  (set-frame-parameter nil 'fullscreen 'maximized)
  (set-frame-font "Ubuntu Mono NF-13"))

(provide 'my-ui)

;;; my-ui.el ends here
