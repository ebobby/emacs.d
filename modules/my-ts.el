;;; my-ts.el --- All about TypeScript
;; Copyright (C) 2010-2021 Francisco Soto
;; Author: Francisco Soto <ebobby@ebobby.org>
;; URL: https://github.com/ebobby/emacs.d
;;
;; This file is not part of GNU Emacs.
;; This file is free software.
;;; Commentary:
;;; Code:

(use-package typescript-ts-mode
  :hook ((typescript-ts-mode . lsp))
  :mode (("\\.ts\\'"  . typescript-ts-mode)
         ("\\.tsx\\'" . typescript-ts-mode)))

(provide 'my-ts)

;;; my-ts.el ends here
