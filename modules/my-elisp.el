;;; my-elisp.el --- Emacs Lisp
;; Copyright (C) 2010-2021 Francisco Soto
;; Author: Francisco Soto <ebobby@ebobby.org>
;; URL: https://github.com/ebobby/emacs.d
;;
;; This file is not part of GNU Emacs.
;; This file is free software.
;;; Commentary:
;;; Code:

(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (rainbow-delimiters-mode)
                                  (rainbow-identifiers-mode)
                                  (rainbow-mode)
                                  (eldoc-mode)
                                  (smartparens-strict-mode)
                                  (setq mode-name "eLisp")))
(provide 'my-elisp)

;;; my-elisp.el ends here
