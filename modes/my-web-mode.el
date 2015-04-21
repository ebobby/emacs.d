;;; my-web-mode.el --- Web mode
;;; Commentary:
;; Web mode for templated html

;;; Code:

(require-packages '(web-mode sass-mode rainbow-mode jsx-mode react-snippets coffee-mode))

(require 'web-mode)
(require 'rainbow-mode)
(require 'jsx-mode)
(require 'coffee-mode)

;; Rainbow mode
(add-hook 'css-mode-hook (lambda () (rainbow-mode 1)))
(add-hook 'html-mode-hook (lambda () (rainbow-mode 1)))
(add-hook 'sass-mode-hook (lambda () (rainbow-mode 1)))
(diminish 'rainbow-mode)

;; Web mode
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 4)

;; JSX mode
(setq jsx-indent-level 2)

;; Coffeescript
(custom-set-variables '(coffee-tab-width 2))

;; What to load with what
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))

(provide 'my-web-mode)

;;; my-web-mode.el ends here
