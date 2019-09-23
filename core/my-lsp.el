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
(require 'lsp-ui)

;; Company lsp completion.
(add-to-list 'company-backends 'company-lsp)

;; Debugging
(add-hook 'lsp-mode-hook (lambda ()
                             (dap-mode t)
                             (dap-ui-mode t)
                             (dap-tooltip-mode t)))

(define-key lsp-ui-mode-map (kbd "M-.") #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map (kbd "M-?") #'lsp-ui-peek-find-references)

(provide 'my-lsp)

;;; my-lsp.el ends here
