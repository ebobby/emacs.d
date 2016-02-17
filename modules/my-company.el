;;; my-company.el --- Company
;;; Commentary:
;; company-mode configuration

;;; Code:

(require-packages '(company))

(setq company-idle-delay .3)
(setq company-tooltip-limit 20)

(add-hook 'after-init-hook 'global-company-mode)

(add-hook 'company-mode-hook  (lambda () (diminish 'company-mode)))

(provide 'my-company)

;;; my-company.el ends here
