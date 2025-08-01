;;; my-web.el --- Web stuff
;; Copyright (C) 2010-2023 Francisco Soto
;; Author: Francisco Soto <ebobby@ebobby.org>
;; URL: https://github.com/ebobby/emacs.d
;;
;; This file is not part of GNU Emacs.
;; This file is free software.
;;; Commentary:
;;; Code:

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.phtml\\'" . web-mode)
         ("\\.erb\\'" . web-mode))
  :hook ((web-mode . lsp))
  :config
  (setq lsp-html-format-enable nil)
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-language-id-configuration
                 '(web-mode . "html"))))

(use-package css-mode
  :hook ((css-mode . lsp)))

(use-package lsp-tailwindcss
  :init (setq lsp-tailwindcss-add-on-mode t)
  :config
  (setq
   lsp-tailwindcss-server-path (executable-find "tailwindcss-language-server")
   lsp-tailwindcss-rustywind-command (executable-find "rustywind")
   lsp-tailwindcss-rustywind-extra-args '("--custom-regex" "\\b(?:class(?:Name)?\\s*[=:]\\s*[\"'])([_a-zA-Z0-9\\.\\s\\-:\\[\\]\\/]+)[\"']")))

(provide 'my-web)

;;; my-web.el ends here
