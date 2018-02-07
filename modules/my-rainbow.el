;;; my-rainbow.el --- Rainbows!
;;; Commentary:
;; rainbow-mode configuration

;;; Code:

(require-packages '(rainbow-mode))

(defun enable-rainbow ()
  (rainbow-mode +1))

(add-hook 'css-mode-hook  'enable-rainbow)
(add-hook 'html-mode-hook 'enable-rainbow)
(add-hook 'sass-mode-hook 'enable-rainbow)
(add-hook 'scss-mode-hook 'enable-rainbow)

(provide 'my-rainbow)

;;; my-rainbow.el ends here
