;;; my-latex.el --- All about Typescript
;;; Commentary:
;; Configure everything about Typescript

;;; Code:

(require-packages '(auctex company-auctex latex-preview-pane))

(require 'company-auctex)
(company-auctex-init)

;; Enable preview
(latex-preview-pane-enable)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-close-quote "")
(setq TeX-open-quote "")

(setq-default TeX-master nil)
(setq reftex-plug-into-AUCTeX t)

;; use pdflatex
(setq TeX-PDF-mode t)

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (rainbow-delimiters-mode)
            (smartparens-mode)
            (turn-on-auto-fill)
            (latex-preview-pane-mode)
            (LaTeX-math-mode)
            (turn-on-reftex)))

(provide 'my-latex)

;;; my-latex.el ends here
