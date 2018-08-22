;;; my-company.el --- Company
;;; Commentary:
;; company-mode configuration

;;; Code:

(require-packages '(company))

(setq company-idle-delay .25)
(setq company-tooltip-limit 20)
(setq company-tooltip-align-annotations t)

(add-hook 'after-init-hook 'global-company-mode)

(add-hook 'company-mode-hook (lambda () (diminish 'company-mode)))

(defun text-mode-hook-setup ()
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends 'company-ispell))

(add-hook 'text-mode-hook 'text-mode-hook-setup)
(add-hook 'org-mode-hook 'text-mode-hook-setup)

(provide 'my-company)

;;; my-company.el ends here
