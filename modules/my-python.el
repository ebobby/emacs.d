;;; my-python.el --- All about Python
;;; Commentary:
;; Configure everything about Python

;;; Code:

(require-packages '(pyenv-mode lsp-python-ms yapfify))

;; Enable pyenv mode first
(pyenv-mode)

(setq lsp-python-ms-auto-install-server t)
(add-hook 'python-mode-hook (lambda ()
                              (yapf-mode)
                              (require 'lsp-python-ms)
                              (lsp)))

;; Use IPython
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

(provide 'my-python)

;;; my-python.el ends here
