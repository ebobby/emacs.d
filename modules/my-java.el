;;; my-java.el --- All about Java
;;; Commentary:
;; Configure everything about Java

;;; Code:

(require-packages '(lsp-java))

(require 'cc-mode)
(require 'lsp-java)

(setq lsp-java-server-install-dir (expand-file-name "java-server"
                                                    utilities-dir))

(add-hook 'java-mode-hook #'lsp)

(provide 'my-java)

;;; my-java.el ends here
