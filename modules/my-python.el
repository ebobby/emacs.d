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
(use-package blacken)

(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp))))

(use-package python
  :hook ((python-mode . dap-mode)
         (python-mode . blacken-mode)
         ((python-mode inferior-python-mode) . setup-python-virtualenv))
  :bind (:map python-mode-map
         ("C-c C-p" . run-python-for-project))
  :config
  (require 'dap-python)

  ;; Temporal fix
  (defun dap-python--pyenv-executable-find (command)
    (with-venv (executable-find "python")))

  (setq dap-python-debugger 'debugpy
        lsp-pyls-plugins-jedi-use-pyenv-environment t
        python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt")

  (defun run-python-for-project ()
    "Run python process in the root of the project."
    (interactive)
    (let ((default-directory (or (helm-ls-git-root-dir) default-directory)))
      (run-python)))

  (defun setup-python-virtualenv ()
    "If a virtualenv is available, use it."
    (let* ((project-dir (helm-ls-git-root-dir))
           (.venv (expand-file-name "venv" project-dir))
           (venv (expand-file-name "venv" project-dir))
           (env (expand-file-name "venv" project-dir))
           (venv-dir (or (when (and venv (file-directory-p venv)) venv)
                         (when (and .venv (file-directory-p venv)) .venv)
                         (when (and env (file-directory-p venv)) env))))
      (if venv-dir
          (pyvenv-activate venv-dir)))))

(provide 'my-python)

;;; my-python.el ends here
