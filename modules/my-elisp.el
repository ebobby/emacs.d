;;; my-elisp.el --- Emacs Lisp
;;; Commentary:
;;

;;; Code:

(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (enable-rainbow-prog)
                                  (eldoc-mode +1)
                                  (smartparens-strict-mode t)
                                  (setq mode-name "eLisp")))

(provide 'my-elisp)

;;; my-elisp.el ends here
