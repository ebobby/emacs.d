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
  :bind (:map js2-mode-map ("M-." . nil))
  :interpreter "node"
  :config
  (require 'dap-node)
  (require 'dap-firefox)
  (setq dap-firefox-debug-program `("node" ,(expand-file-name "extension/dist/adapter.bundle.js"
                                                              dap-firefox-debug-path))
        js-chain-indent t
        js2-basic-offset 2
        js2-highlight-external-variables t
        js2-highlight-level 3
        js2-idle-timer-delay 0.1
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        js2-skip-preprocessor-directives t
        js2-strict-missing-semi-warning nil
        js2-strict-trailing-comma-warning nil)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package nodejs-repl
  :bind (:map js2-mode-map
              ("C-x C-e" . nodejs-repl-send-last-expression)
              ("C-c C-j" . nodejs-repl-send-line)
              ("C-c C-r" . nodejs-repl-send-region)
              ("C-c C-c" . nodejs-repl-send-buffer)
              ("C-c C-l" . nodejs-repl-load-file)
              ("C-c C-z" . nodejs-repl-switch-to-repl)))

(use-package rjsx-mode
  :bind (:map rjsx-mode-map ("C-c C-r" . nodejs-repl-send-region))
  :mode (("\\.js\\'"  . rjsx-mode)
         ("\\.jsx\\'" . rjsx-mode)))

(use-package prettier-js
  :hook ((js2-mode js2-jsx-mode) . prettier-js-mode))

(provide 'my-js)

;;; my-js.el ends here
