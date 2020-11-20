;;; my-js.el --- All about JS
;;; Commentary:
;; Configure everything about JS

;;; Code:

(require-packages '(js2-mode
                    js-comint
                    prettier-js
                    nvm))

(require 'js2-mode)
(require 'js-comint)
(require 'prettier-js)
(require 'nvm)

(add-to-list 'auto-mode-alist '("\\.js\\'"  . js2-mode))
(add-to-list 'auto-mode-alist '("\\.pac\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))

(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(setq-default js2-basic-offset 2)

(defun setup-js2 ()
  (setq-local electric-layout-rules '((?\; . after)))
  (js2-imenu-extras-mode +1)
  (prettier-js-mode +1)
  (make-local-variable 'company-backends))

(add-hook 'js2-mode-hook 'setup-js2)
(add-hook 'js2-jsx-mode-hook 'setup-js2)

;; Override emacs jsx mode.
;(setq 'js-jsx-mode js2-jsx-mode)

;; Run LSP if available.
(add-hook 'js2-mode-hook 'lsp)

(require 'dap-node)

(setq js2-strict-missing-semi-warning nil)

(provide 'my-js)

;;; my-js.el ends here
