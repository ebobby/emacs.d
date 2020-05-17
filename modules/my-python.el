;;; my-python.el --- All about Python
;;; Commentary:
;; Configure everything about Python

;;; Code:

(require-packages '(elpy py-autopep8 blacken pyenv-mode))

;; Enable pyenv mode first
(pyenv-mode)

;; Enable elpy
(elpy-enable)

;; Enable Flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Enable autopep8
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; Use IPython
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

(provide 'my-python)

;;; my-python.el ends here
