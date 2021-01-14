;;; my-python.el --- All about Python
;;; Commentary:
;; Configure everything about Python

;;; Code:

(require-packages '(pyvenv pyenv-mode lsp-python-ms py-autopep8))

(defun run-python-for-project ()
  "Run python process in the root of the project."
  (interactive)
  (let ((default-directory (or (helm-ls-git-root-dir)
                               default-directory)))
    (run-python)))

(defun setup-python-virtualenv ()
  "If a virtualenv is available, use it."
  (let* ((project-dir (helm-ls-git-root-dir))
         (venv-dir (or (expand-file-name "venv" project-dir)
                       (expand-file-name "env" project-dir))))
    (if (file-directory-p venv-dir)
        (pyvenv-activate venv-dir))))

;; Enable pyenv mode first
(pyenv-mode)

(add-hook 'python-mode-hook (lambda ()
                              (require 'lsp-python-ms)
                              (setup-python-virtualenv)
                              (py-autopep8-enable-on-save)
                              (lsp)))

;; Auto install Microsoft's Python Server
(setq lsp-python-ms-auto-install-server t)

;; Use IPython
(setq python-shell-interpreter      "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

;; Run python repl at the root of the project if possible.
(define-key python-mode-map (kbd "C-c C-p") 'run-python-for-project)

(provide 'my-python)

;;; my-python.el ends here
