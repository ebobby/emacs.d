;;; my-haskell.el --- All about Haskell
;;; Commentary:
;; Configure everything about Haskell

;;; Code:

(require-packages '(haskell-mode intero flycheck-haskell))

;; Haskell
(require 'haskell-mode)

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'intero-mode)

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

(provide 'my-haskell)

;;; my-haskell.el ends here
