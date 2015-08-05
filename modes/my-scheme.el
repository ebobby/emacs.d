;;; my-scheme.el --- All about Scheme
;;; Commentary:
;; Configure everything about Scheme

;;; Code:

(require-packages '(geiser paredit))

(setq geiser-active-implementations '(chicken))

;; Paredit
(require 'paredit)
(add-hook 'scheme-mode-hook           'enable-paredit-mode)
(diminish 'paredit-mode)


(provide 'my-scheme)

;;; my-scheme.el ends here
