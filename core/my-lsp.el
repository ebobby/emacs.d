;;; my-lsp.el --- Lsp
;;; Commentary:
;; lsp-mode configuration

;;; Code:

(require-packages '(lsp-mode
                    lsp-ui
                    dap-mode
                    helm-lsp
                    company-lsp))
(require 'lsp-mode)
(require 'dap-mode)

;; Company lsp completion.
(add-to-list 'company-backends 'company-lsp)

;; Debugging
(add-hook 'lsp-mode-hook (lambda ()
                             (dap-mode t)
                             (dap-ui-mode t)))

(provide 'my-lsp)

;;; my-lsp.el ends here
