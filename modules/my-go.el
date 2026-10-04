;;; my-go.el --- All about Go  -*- lexical-binding: t; -*-
;; Copyright (C) 2010-2023 Francisco Soto
;; Author: Francisco Soto <ebobby@ebobby.org>
;; URL: https://github.com/ebobby/emacs.d
;;
;; This file is not part of GNU Emacs.
;; This file is free software.
;;; Commentary:
;;; Code:

(defun my-go-setup ()
  "Format and organize imports with gopls on save."
  (add-hook 'before-save-hook #'lsp-format-buffer nil t)
  (add-hook 'before-save-hook #'lsp-organize-imports nil t))

(use-package go-ts-mode
  :hook ((go-ts-mode . lsp-deferred)
         (go-ts-mode . my-go-setup))
  :mode (("\\.go\\'" . go-ts-mode)
         ("/go\\.mod\\'" . go-mod-ts-mode))
  :config
  (with-eval-after-load 'dap-mode
    (require 'dap-dlv-go))
  (setq lsp-go-hover-kind "FullDocumentation"
        lsp-go-use-gofumpt t))

(provide 'my-go)

;;; my-go.el ends here
