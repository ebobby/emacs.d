;;; my-rainbow.el --- Rainbows!
;;; Commentary:
;; rainbow-mode configuration

;;; Code:

(require-packages '(rainbow-mode rainbow-delimiters rainbow-identifiers))

(require 'rainbow-delimiters)
(require 'rainbow-identifiers)

(defun enable-rainbow ()
  (rainbow-mode t))

(defun enable-rainbow-prog ()
  (rainbow-delimiters-mode t)
  (rainbow-identifiers-mode t) )

(add-hook 'prog-mode-hook 'enable-rainbow-prog)

(add-hook 'css-mode-hook  'enable-rainbow)
(add-hook 'html-mode-hook 'enable-rainbow)
(add-hook 'sass-mode-hook 'enable-rainbow)
(add-hook 'scss-mode-hook 'enable-rainbow)

(provide 'my-rainbow)

;;; my-rainbow.el ends here
