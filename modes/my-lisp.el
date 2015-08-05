;;; my-lisp.el --- All about CL development
;;; Commentary:
;; Configure everything about Common Lisp

;;; Code:

(require-packages '(slime paredit))

;; Common Lisp
(require 'slime)
(require 'slime-autoloads)

(add-hook 'slime-repl-mode-hook
          (lambda () (local-set-key (kbd "C-c C-]") 'slime-close-all-parens-in-sexp)))

(push '("\\.asd$" . lisp-mode) auto-mode-alist)

(slime-setup '(slime-fancy slime-repl slime-editing-commands slime-references slime-autodoc slime-fancy-inspector))

;; Paredit
(require 'paredit)
(add-hook 'emacs-lisp-mode-hook       'enable-paredit-mode)
(add-hook 'lisp-mode-hook             'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook       'enable-paredit-mode)
(diminish 'paredit-mode)

(defun run-sbcl ()
  "Start SLIME with Steel Bank CL."
  (interactive)
  (let ((inferior-lisp-program "/usr/local/bin/sbcl"))
    (slime)))

(defun run-ccl ()
  "Start SLIME with Clozure CL."
  (interactive)
  (let ((inferior-lisp-program "/usr/local/bin/ccl64"))
    (slime)))

(defun run-clisp ()
  "Start SLIME with CLisp."
  (interactive)
  (let ((inferior-lisp-program "/usr/local/bin/clisp"))
    (slime)))

(provide 'my-lisp)

;;; my-lisp.el ends here
