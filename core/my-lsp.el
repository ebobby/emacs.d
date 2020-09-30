;;; my-lsp.el --- All about Clojure
;;; Commentary:
;; Configure everything about Clojure

;;; Code:

(require-packages '(lsp-mode
                    lsp-ui
                    helm-lsp
                    dap-mode))

;; Language Server Protocol
(setq lsp-keymap-prefix "C-c l")

(require 'lsp-ui)
(require 'lsp-mode)

(provide 'my-lsp)

;;; my-lsp.el ends here
