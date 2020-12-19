;;; my-js.el --- All about JS
;;; Commentary:
;; Configure everything about JS

;;; Code:

(require-packages '(js2-mode
                    js-comint
                    prettier-js
                    rjsx-mode
                    nvm))

(require 'js)
(require 'js2-mode)
(require 'rjsx-mode)
(require 'js-comint)
(require 'prettier-js)
(require 'nvm)

(add-to-list 'auto-mode-alist '("\\.js\\'"  . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))

(add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(setq js2-basic-offset 2
      js-chain-indent t
      ;; Don't mishighlight shebang lines
      js2-skip-preprocessor-directives t
      ;; let flycheck handle this
      js2-mode-show-parse-errors nil
      js2-mode-show-strict-warnings nil
      ;; Flycheck provides these features, so disable them: conflicting with
      ;; the eslint settings.
      js2-strict-trailing-comma-warning nil
      js2-strict-missing-semi-warning nil
      ;; maximum fontification
      js2-highlight-level 3
      js2-highlight-external-variables t
      js2-idle-timer-delay 0.1)

(defun setup-js2 ()
  "Setup JS."
  (setq electric-layout-rules '((?\; . after)))
  (js2-imenu-extras-mode +1)
  (prettier-js-mode +1)
  (make-local-variable 'company-backends))

(add-hook 'js2-mode-hook 'setup-js2)
(add-hook 'js2-jsx-mode-hook 'setup-js2)

;; Override emacs jsx mode.
(fset 'js-jsx-mode 'rjsx-mode)

;; Run LSP if available.
(add-hook 'js2-mode-hook 'lsp)

(require 'dap-node)

(provide 'my-js)

;;; my-js.el ends here
