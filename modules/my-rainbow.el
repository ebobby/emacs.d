;;; my-rainbow.el --- Rainbows!
;;; Commentary:
;; rainbow-mode configuration

;;; Code:

(require-packages '(rainbow-mode))

(add-hook 'css-mode-hook  (lambda () (rainbow-mode 1)))
(add-hook 'html-mode-hook (lambda () (rainbow-mode 1)))
(add-hook 'sass-mode-hook (lambda () (rainbow-mode 1)))

(provide 'my-rainbow)

;;; my-rainbow.el ends here
