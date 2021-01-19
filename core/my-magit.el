;;; my-magit.el --- All about Magit
;;; Commentary:
;; Configure everything about Magit and Git

;;; Code:

(require-packages '(magit))

;; Git
(require 'magit)
(autoload 'magit-status "magit" nil t)
(setq magit-auto-revert-mode nil)

(global-set-key (kbd "<f10>") 'magit-status)
;;(define-key my-mode-map (kbd "C-c m l") 'magit-log)
;;(define-key my-mode-map (kbd "C-c m f") 'magit-log-buffer-file)
;;(define-key my-mode-map (kbd "C-c m b") 'magit-blame)
;;(define-key my-mode-map (kbd "C-c m m") 'magit-status)

(setq magit-last-seen-setup-instructions "1.4.0")

(provide 'my-magit)

;;; my-magit.el ends here
