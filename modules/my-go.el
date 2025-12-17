;;; my-go.el --- All about Go
;; Copyright (C) 2010-2023 Francisco Soto
;; Author: Francisco Soto <ebobby@ebobby.org>
;; URL: https://github.com/ebobby/emacs.d
;;
;; This file is not part of GNU Emacs.
;; This file is free software.
;;; Commentary:
;;; Code:

(use-package go-ts-mode
  :hook ((go-ts-mode . lsp-deferred))
  :mode (("\\.go\\'" . go-ts-mode)
         ("/go\\.mod\\'" . go-mod-ts-mode))
  :config
  (require 'dap-dlv-go)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq gofmt-args '("-s")
        lsp-go-hover-kind "FullDocumentation"))

(provide 'my-go)

;;; my-go.el ends here
