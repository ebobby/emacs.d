;;; my-text.el --- Text writing
;;; Commentary:
;; Textile, Markdown, etc.

;;; Code:

(require-packages '(markdown-mode yaml-mode))

;; Markdown
(require 'markdown-mode)
(push '("\\.md$" . markdown-mode) auto-mode-alist)
(push '("\\.markdown$" . markdown-mode) auto-mode-alist)

;; YAML
(require 'yaml-mode)

(provide 'my-text)

;;; my-text.el ends here
