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

(use-package js
  :hook ((js-ts-mode . lsp)
         (js-ts-mode . dap-mode)
         (typescript-ts-mode . lsp)
         (typescript-ts-mode . dap-mode))
  :mode (("\\.js\\'"  . js-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.ts\\'"  . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)
         ("\\.json\\'" . json-ts-mode))
  :interpreter "node"
  :config
  (require 'dap-node)
  (require 'lsp-javascript)
  (setq js-indent-level 2))

(use-package nodejs-repl
  :bind (:map js-ts-mode-map
              ("C-x C-e" . nodejs-repl-send-last-expression)
              ("C-c C-j" . nodejs-repl-send-line)
              ("C-c C-r" . nodejs-repl-send-region)
              ("C-c C-c" . nodejs-repl-send-buffer)
              ("C-c C-l" . nodejs-repl-load-file)
              ("C-c C-z" . nodejs-repl-switch-to-repl)))

(use-package npm-mode
  :hook ((js-ts-mode tsx-ts-mode typescript-ts-mode) . npm-mode))

(use-package prettier-js
  :hook ((js-ts-mode tsx-ts-mode typescript-ts-mode) . prettier-js-mode))

(provide 'my-js)

;;; my-js.el ends here
