;;; my-haskell.el --- All about Haskell
;;; Commentary:
;; Configure everything about Haskell

;;; Code:

(require-packages '(haskell-mode))

;; Haskell
(require 'haskell-mode)

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(provide 'my-haskell)

;;; my-haskell.el ends here
