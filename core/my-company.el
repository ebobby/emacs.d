;;; my-company.el --- Company
;;; Commentary:
;; company-mode configuration

;;; Code:

(require-packages '(company))

(require 'company)

(setq company-idle-delay 0.0)
(setq company-minimum-prefix-length 1)
;;(setq company-tooltip-align-annotations t)
;;(setq company-tooltip-limit 20)

(add-hook 'after-init-hook 'global-company-mode)

(defun text-mode-hook-setup ()
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends 'company-ispell))

(add-hook 'text-mode-hook 'text-mode-hook-setup)
(add-hook 'org-mode-hook 'text-mode-hook-setup)

(provide 'my-company)

;;; my-company.el ends here
