;;; my-js.el --- All about JS
;; Copyright (C) 2010-2021 Francisco Soto
;; Author: Francisco Soto <ebobby@ebobby.org>
;; URL: https://github.com/ebobby/emacs.d
;;
;; This file is not part of GNU Emacs.
;; This file is free software.
;;; Commentary:
;;; Code:

(use-package nvm)

(use-package js2-mode
  :hook (((js2-mode js2-mode-jsx) . js2-imenu-extras-mode)
         (js2-mode . lsp)
         (js2-mode . dap-mode))
  :interpreter "node"
  :config
  (require 'dap-node)
  (setq js-chain-indent t
        js2-basic-offset 2
        js2-highlight-external-variables t
        js2-highlight-level 3
        js2-idle-timer-delay 0.1
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        js2-skip-preprocessor-directives t
        js2-strict-missing-semi-warning nil
        js2-strict-trailing-comma-warning nil))

(use-package js-comint)

(use-package prettier-js
  :hook ((js2-mode js2-jsx-mode) . prettier-js-mode))

(use-package rjsx-mode
  :mode (("\\.js\\'"  . rjsx-mode)
         ("\\.jsx\\'" . rjsx-mode)))

;; Override emacs jsx mode.
;;(fset 'js-jsx-mode 'rjsx-mode)

(provide 'my-js)

;;; my-js.el ends here
