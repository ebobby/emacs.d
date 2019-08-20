;;; my-elisp.el --- Emacs Lisp
;;; Commentary:
;;

;;; Code:

(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                   (eldoc-mode +1)
                                   (rainbow-mode +1)
                                   (smartparens-strict-mode +1)
                                   (setq mode-name "ELisp")))

(eval-after-load "eldoc"
  '(diminish 'eldoc-mode))

(provide 'my-elisp)

;;; my-elisp.el ends here
