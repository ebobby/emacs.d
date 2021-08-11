;;; my-latex.el --- All about Latex
;; Copyright (C) 2010-2021 Francisco Soto
;; Author: Francisco Soto <ebobby@ebobby.org>
;; URL: https://github.com/ebobby/emacs.d
;;
;; This file is not part of GNU Emacs.
;; This file is free software.
;;; Commentary:
;;; Code:

(use-package helm-bibtex)

(use-package auctex
  :defer t
  :ensure t
  :hook (LaTeX-mode . lsp)
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-close-quote "")
  (setq TeX-open-quote "")
  (setq-default TeX-master nil)
  (setq reftex-plug-into-AUCTeX t)

  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (rainbow-delimiters-mode)
              (smartparens-mode)
              (turn-on-auto-fill)
              (LaTeX-math-mode)
              (turn-on-reftex))))

(use-package company-auctex
  :config (company-auctex-init))

(provide 'my-latex)

;;; my-latex.el ends here
