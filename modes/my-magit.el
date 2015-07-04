;;; my-magit.el --- All about Magit
;;; Commentary:
;; Configure everything about Magit and Git

;;; Code:

(require-packages '(magit))

;; Git
(require 'magit)
(autoload 'magit-status "magit" nil t)
(add-hook 'magit-mode-hook (lambda () (setq-local yas-dont-activate t)))
(setq magit-auto-revert-mode nil)

(setq magit-last-seen-setup-instructions "1.4.0")

(provide 'my-magit)

;;; my-magit.el ends here
