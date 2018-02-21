;;; my-haskell.el --- All about Haskell
;;; Commentary:
;; Configure everything about Haskell

;;; Code:

(require-packages '(haskell-mode intero haskell-snippets))

;; Haskell
(require 'haskell-mode)

;; Install Haskell dash docset
(my-install-official-docset "Haskell")

(add-hook 'haskell-mode-hook (lambda ()
                               (setq-local helm-dash-docsets '("Haskell"))
                               (haskell-indentation-mode 1)
                               (haskell-doc-mode 1)
                               (intero-mode 1)))

(provide 'my-haskell)

;;; my-haskell.el ends here
