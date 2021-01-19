;;; my-lisp.el --- All about CL development
;;; Commentary:
;; Configure everything about Common Lisp

;;; Code:

(require-packages '(slime common-lisp-snippets))

;; Common Lisp
(require 'slime)
(require 'slime-autoloads)

(add-hook 'slime-repl-mode-hook
          (lambda () (local-set-key (kbd "C-c C-]") 'slime-close-all-parens-in-sexp)))

(push '("\\.asd$" . lisp-mode) auto-mode-alist)

(slime-setup '(slime-fancy slime-repl slime-editing-commands slime-references slime-autodoc slime-fancy-inspector))

(add-hook 'emacs-elisp-mode-hook (lambda ()
                                   (eldoc-mode +1)
                                   (rainbow-mode)
                                   (smartparens-strict-mode +1)))

(add-hook 'emacs-lisp-mode-hook (lambda () (smartparens-strict-mode +1)))
(add-hook 'lisp-mode-hook (lambda () (smartparens-strict-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (smartparens-strict-mode +1)))
(add-hook 'slime-repl-mode-hook (lambda () (smartparens-strict-mode +1)))

(defun run-sbcl ()
  "Start SLIME with Steel Bank CL."
  (interactive)
  (let ((inferior-lisp-program "/usr/local/bin/sbcl"))
    (slime)))

(provide 'my-lisp)

;;; my-lisp.el ends here
