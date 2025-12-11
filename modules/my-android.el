;;; my-android.el --- All about Android
;; Copyright (C) 2010-2025 Francisco Soto
;; Author: Francisco Soto <ebobby@ebobby.org>
;; URL: https://github.com/ebobby/emacs.d
;;
;; This file is not part of GNU Emacs.
;; This file is free software.
;;; Commentary:
;;; Code:

(use-package kotlin-ts-mode
  :hook ((kotlin-ts-mode . lsp-deferred))
  :mode (("\\.kt\\'" . kotlin-ts-mode)
         ("/go\\.mod\\'" . kotlin-ts-mode)))

(provide 'my-android)

;;; my-android.el ends here
