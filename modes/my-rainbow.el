;;; my-rainbow.el --- Nice colors
;;; Commentary:
;; Nice colors when editing css and  html.

;;; Code:

(require-packages '(rainbow-mode))

;; Rainbow mode
(require 'rainbow-mode)
(add-hook 'css-mode-hook (lambda () (rainbow-mode 1)))
(add-hook 'html-mode-hook (lambda () (rainbow-mode 1)))

(provide 'my-rainbow)

;;; my-rainbow.el ends here
