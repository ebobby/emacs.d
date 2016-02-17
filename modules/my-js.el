;;; my-js.el --- All about JS
;;; Commentary:
;; Configure everything about JS

;;; Code:

(require-packages '(js2-mode json-mode))

(require 'js2-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'"    . js2-mode))
(add-to-list 'auto-mode-alist '("\\.pac\\'"   . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(add-hook 'js2-mode-hook '(lambda ()
                            ;; electric-layout-mode doesn't play nice with smartparens
                            (setq-local electric-layout-rules '((?\; . after)))
                            (setq mode-name "JS2")
                            (js2-imenu-extras-mode +1)))

(setq-default js2-basic-offset 2)

(provide 'my-js)

;;; my-js.el ends here
