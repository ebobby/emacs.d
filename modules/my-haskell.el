;;; my-haskell.el --- All about Haskell
;; Copyright (C) 2010-2021 Francisco Soto
;; Author: Francisco Soto <ebobby@ebobby.org>
;; URL: https://github.com/ebobby/emacs.d
;;
;; This file is not part of GNU Emacs.
;; This file is free software.
;;; Commentary:
;;; Code:

(use-package lsp-haskell)

(use-package haskell-mode
  :hook ((haskell-mode . lsp)
         (haskell-literate-mode . lsp))
  :mode "\\.hs\\'")

(provide 'my-haskell)

;;; my-haskell.el ends here
