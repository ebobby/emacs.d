;;; my-haskell.el --- All about Haskell
;; Copyright (C) 2010-2021 Francisco Soto
;; Author: Francisco Soto <ebobby@ebobby.org>
;; URL: https://github.com/ebobby/emacs.d
;;
;; This file is not part of GNU Emacs.
;; This file is free software.
;;; Commentary:
;;; Code:

(use-package lsp-haskell
  :config
  (require 'lsp-haskell))

(defun haskell-setup ()
  "Setup Haskell-related modes."
  (subword-mode +1)
  (eldoc-mode +1)
  (haskell-indentation-mode +1)
  (interactive-haskell-mode +1)
  (lsp))

(use-package haskell-mode
  :hook ((haskell-mode . haskell-setup)
         (haskell-literate-mode . haskell-setup))
  :mode "\\.hs\\'")

(provide 'my-haskell)

;;; my-haskell.el ends here
