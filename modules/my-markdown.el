;;; my-markdown.el --- Markdown editing
;;; Commentary:
;; Markdown mode

;;; Code:

(require-packages '(markdown-mode))

(require 'markdown-mode)

(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

(setq markdown-command "multimarkdown")

(provide 'my-markdown)

;;; my-markdown.el ends here
