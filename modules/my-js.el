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

(use-package json-ts-mode
  :mode (("\\.json\\'" . json-ts-mode)))

(use-package js-ts-mode
  :hook ((js-ts-mode . lsp)
         (js-ts-mode . dap-mode))
  :mode (("\\.js\\'"  . js-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode))
  :interpreter "node"
  :config
  (require 'dap-node)
  (require 'dap-firefox)
  (setq dap-firefox-debug-program `("node" ,(expand-file-name "extension/dist/adapter.bundle.js"
                                                              dap-firefox-debug-path))))

(use-package nodejs-repl
  :bind (:map js-ts-mode-map
              ("C-x C-e" . nodejs-repl-send-last-expression)
              ("C-c C-j" . nodejs-repl-send-line)
              ("C-c C-r" . nodejs-repl-send-region)
              ("C-c C-c" . nodejs-repl-send-buffer)
              ("C-c C-l" . nodejs-repl-load-file)
              ("C-c C-z" . nodejs-repl-switch-to-repl)))

(use-package npm-mode
  :hook ((js-ts-mode tsx-ts-mode) . npm-mode))

(use-package prettier-js
  :hook ((js-ts-mode tsx-ts-mode) . prettier-js-mode))

(use-package typescript-ts-mode
  :hook ((typescript-ts-mode . lsp))
  :mode (("\\.ts\\'"  . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)))

(provide 'my-js)

;;; my-js.el ends here
