;;; my-latex.el --- All about Typescript
;;; Commentary:
;; Configure everything about Typescript

;;; Code:

(require-packages '(auctex))

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-close-quote "")
(setq TeX-open-quote "")

(setq-default TeX-master nil)

;; use pdflatex
(setq TeX-PDF-mode t)

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (rainbow-delimiters-mode)
            (smartparens-mode)
            (turn-on-auto-fill)))

(provide 'my-latex)

;;; my-latex.el ends here
