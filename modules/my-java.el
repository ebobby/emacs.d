;;; my-java.el --- All about Java
;;; Commentary:
;; Configure everything about Java

;;; Code:

(require-packages '(lsp-java))

(require 'cc-mode)
(require 'lsp-java)
(require 'dap-java)

(setq lsp-java-server-install-dir (expand-file-name "java-server"
                                                    utilities-dir))

(setq lsp-prefer-flymake nil)
(setq lsp-ui-doc-enable nil
      ;lsp-ui-sideline-enable nil
      lsp-ui-flycheck-enable t)

(dap-mode t)
(dap-ui-mode t)

;; Enable dap-java
(require 'dap-java)

(add-hook 'java-mode-hook #'lsp)
(add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 4
                                  tab-width 4
                                  indent-tabs-mode t)))
(provide 'my-java)

;;; my-java.el ends here
