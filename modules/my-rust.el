;;; my-rust.el --- All about Rust  -*- lexical-binding: t; -*-
;; Copyright (C) 2010-2021 Francisco Soto
;; Author: Francisco Soto <ebobby@ebobby.org>
;; URL: https://github.com/ebobby/emacs.d
;;
;; This file is not part of GNU Emacs.
;; This file is free software.
;;; Commentary:
;;; Code:

(use-package cargo)

(use-package rust-ts-mode
  :hook ((rust-ts-mode . lsp-deferred)
         (rust-ts-mode . cargo-minor-mode)
         (rust-ts-mode . subword-mode))
  :mode "\\.rs\\'"
  :bind (:map rust-ts-mode-map
              ("C-c C-d" . lsp-describe-thing-at-point)))

(provide 'my-rust)

;;; my-rust.el ends here
