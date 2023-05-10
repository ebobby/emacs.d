;;; my-web.el --- Web stuff
;; Copyright (C) 2010-2023 Francisco Soto
;; Author: Francisco Soto <ebobby@ebobby.org>
;; URL: https://github.com/ebobby/emacs.d
;;
;; This file is not part of GNU Emacs.
;; This file is free software.
;;; Commentary:
;;; Code:

(use-package mhtml-mode
  :hook ((mhtml-mode . lsp)
         (mhtml-mode . (lambda ()
                         (local-unset-key (kbd "M-o"))))))
(use-package css-mode
  :hook ((css-mode . lsp)))

(provide 'my-web)

;;; my-web.el ends here
