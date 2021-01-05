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
(require 'dap-mode)
(dap-mode 1)

(setq lsp-auto-configure t)
(setq lsp-completion-provider :capf)
(setq lsp-ui-doc-position 'at-point)

(define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)

(provide 'my-lsp)

;;; my-lsp.el ends here
