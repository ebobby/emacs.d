;;; my-python.el --- All about Python
;; Copyright (C) 2010-2021 Francisco Soto
;; Author: Francisco Soto <ebobby@ebobby.org>
;; URL: https://github.com/ebobby/emacs.d
;;
;; This file is not part of GNU Emacs.
;; This file is free software.
;;; Commentary:
;;; Code:

(use-package pyenv-mode
  :config
  (pyenv-mode))

(use-package pyvenv)
(use-package with-venv)

(use-package lsp-pyright
  :ensure t
  :config
  (require 'lsp-pyright)
  (setq lsp-file-watch-threshold 10000
        ;;lsp-pyright-diagnostic-mode "workspace"
        ))

(use-package python
  :mode (("\\.py\\'" . python-ts-mode))
  :hook ((python-ts-mode . dap-mode)
         (python-ts-mode . lsp)
         (python-ts-mode . (lambda () (setq-local lsp-diagnostics-provider :none)))
         ((python-ts-mode inferior-python-mode) . setup-python-virtualenv))
  :bind (:map python-ts-mode-map
         ("C-c C-p" . run-python-for-project))
  :config
  (require 'dap-python)

  (setf
   (flycheck-checker-get 'python-flake8 'modes) '(python-mode python-ts-mode)
   (flycheck-checker-get 'python-pylint 'modes) '(python-mode python-ts-mode)
   (flycheck-checker-get 'python-pycompile 'modes) '(python-mode python-ts-mode)
   (flycheck-checker-get 'python-pyright 'modes) '(python-mode python-ts-mode)
   (flycheck-checker-get 'python-mypy 'modes) '(python-mode python-ts-mode))

  ;; Temporal fix
  (defun dap-python--pyenv-executable-find (command)
    (with-venv (executable-find "python")))

  (setq dap-python-debugger 'debugpy
        lsp-pyls-plugins-jedi-use-pyenv-environment t
        python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt"
        ;; Flycheck assumes python3 is always the correct binary. Gotta fix it.
        flycheck-python-flake8-executable "python"
        flycheck-flake8rc ".flake8"
        flycheck-python-pylint-executable "python")

  (defun run-python-for-project ()
    "Run python process in the root of the project."
    (interactive)
    (let ((default-directory (or (helm-ls-git-root-dir) default-directory)))
      (run-python)))

  (defun setup-python-virtualenv ()
    "If a `virtualenv' is available, use it."
    (let* ((project-dir (helm-ls-git-root-dir))
           (venv-dir nil))
      (dolist (venv '("venv" ".venv" "env"))
        (let ((potential-venv (expand-file-name venv project-dir)))
          (if (and potential-venv (file-directory-p potential-venv))
              (setq venv-dir potential-venv))))
      (when venv-dir
        (message (format "Activating virtualenv: %s" venv-dir))
        (pyvenv-activate venv-dir)))))

(provide 'my-python)

;;; my-python.el ends here
