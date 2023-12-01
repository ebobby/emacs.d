;;; my-go.el --- All about Go
;; Copyright (C) 2010-2023 Francisco Soto
;; Author: Francisco Soto <ebobby@ebobby.org>
;; URL: https://github.com/ebobby/emacs.d
;;
;; This file is not part of GNU Emacs.
;; This file is free software.
;;; Commentary:
;;; Code:

(use-package go-mode
  :hook ((go-mode . lsp-deferred))
  :mode (("\\.go\\'" . go-mode)
         ("/go\\.mod\\'" . go-mode))
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq gofmt-args '("-s")
        lsp-go-hover-kind "FullDocumentation"))

(provide 'my-go)

;;; my-go.el ends here
