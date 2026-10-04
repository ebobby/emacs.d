;;; my-js.el --- All about JS  -*- lexical-binding: t; -*-
;; Copyright (C) 2010-2021 Francisco Soto
;; Author: Francisco Soto <ebobby@ebobby.org>
;; URL: https://github.com/ebobby/emacs.d
;;
;; This file is not part of GNU Emacs.
;; This file is free software.
;;; Commentary:
;;; Code:

(use-package js
  :ensure nil
  :hook (((js-ts-mode typescript-ts-base-mode) . lsp-deferred)
         ((js-ts-mode typescript-ts-base-mode) . dap-mode))
  :mode (("\\.[cm]?jsx?\\'" . js-ts-mode)
         ("\\.ts\\'"  . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)
         ("\\.json\\'" . json-ts-mode))
  :interpreter ("node" . js-ts-mode)
  :config
  (with-eval-after-load 'dap-mode
    (require 'dap-node))
  (setq js-chain-indent t
        js-indent-level 2))

(use-package npm-mode
  :hook ((js-ts-mode typescript-ts-base-mode) . npm-mode))

(use-package prettier-js
  :hook ((js-ts-mode typescript-ts-base-mode) . prettier-js-mode))

(provide 'my-js)

;;; my-js.el ends here
