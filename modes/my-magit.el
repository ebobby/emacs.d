;;; my-magit.el --- All about Magit
;;; Commentary:
;; Configure everything about Magit and Git

;;; Code:

(require-packages '(magit))

;; Git
(require 'magit)
(autoload 'magit-status "magit" nil t)
(add-hook 'magit-mode-hook (lambda () (setq-local yas-dont-activate t)))

(eval-after-load 'magit
  '(progn
     (set-face-background 'highlight nil) ;; highlight is overriding other background colors for diff chunks
     (set-face-foreground 'highlight nil) ;; highlight is overriding other foreground colors for diff chunks
     (set-face-underline  'highlight nil)
     (define-key magit-mode-map (kbd "<tab>") 'magit-toggle-section)    ; was smart-tab
     (define-key magit-status-mode-map (kbd "M-K") 'magit-quit-session)
     (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace) ))

(provide 'my-magit)

;;; my-magit.el ends here
