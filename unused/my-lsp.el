;;; my-lsp.el --- Lsp
;;; Commentary:
;; lsp-mode configuration

;;; Code:

(require-packages '(company-lsp
                    dap-mode
                    helm-lsp
                    lsp-mode
                    lsp-ui))
(require 'dap-mode)
(require 'lsp-mode)
(require 'lsp-ui)
(require 'lsp-clients)

;; Company lsp completion.
(add-to-list 'company-backends 'company-lsp)

(setq lsp-prefer-flymake nil)
(setq lsp-ui-doc-enable nil
      lsp-ui-sideline-enable nil
      lsp-ui-flycheck-enable t)

(dap-mode t)
;(dap-ui-mode t)

;; Debugging
3(add-hook 'lsp-mode-hook (lambda ()
                             (dap-mode t)
                             (dap-ui-mode t)
                             (dap-tooltip-mode t)))

(define-key lsp-ui-mode-map (kbd "M-.") #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map (kbd "M-?") #'lsp-ui-peek-find-references)

(provide 'my-lsp)

;;; my-lsp.el ends here
