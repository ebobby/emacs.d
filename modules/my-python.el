;;; my-python.el --- All about Python
;;; Commentary:
;; Configure everything about Python

;;; Code:

(require-packages '(pyenv-mode lsp-python-ms py-autopep8))

;; Enable pyenv mode first
(pyenv-mode)

(add-hook 'python-mode-hook (lambda ()
                              (py-autopep8-enable-on-save)
                              (require 'lsp-python-ms)
                              (lsp)))

;; Auto install Microsoft's Python Server
(setq lsp-python-ms-auto-install-server t)

;; Use IPython
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

(provide 'my-python)

;;; my-python.el ends here
