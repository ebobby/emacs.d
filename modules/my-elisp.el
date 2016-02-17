;;; my-elisp.el --- Emacs Lisp
;;; Commentary:
;;

;;; Code:

(require-packages '(rainbow-mode elisp-slime-nav))

(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (eldoc-mode +1)
                                  (rainbow-mode +1)
                                  (elisp-slime-nav-mode +1)
                                  (smartparens-global-strict-mode +1)
                                  (setq mode-name "ELisp")))

(eval-after-load "elisp-slime-nav"
  '(diminish 'elisp-slime-nav-mode))
(eval-after-load "rainbow-mode"
  '(diminish 'rainbow-mode))
(eval-after-load "eldoc"
  '(diminish 'eldoc-mode))

(provide 'my-elisp)

;;; my-elisp.el ends here
