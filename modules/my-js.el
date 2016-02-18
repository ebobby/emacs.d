;;; my-js.el --- All about JS
;;; Commentary:
;; Configure everything about JS

;;; Code:

(require-packages '(js2-mode tern company-tern))

(require 'js2-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'"    . js2-mode))
(add-to-list 'auto-mode-alist '("\\.pac\\'"   . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'"   . js2-jsx-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(setq-default js2-basic-offset 2)

(defun setup-js2 ()
  ;; electric-layout-mode doesn't play nice with smartparens
  (setq-local electric-layout-rules '((?\; . after)))
  (js2-imenu-extras-mode +1)
  (tern-mode t)
  (unless (member 'company-tern 'company-backends)
    (add-to-list 'company-backends 'company-tern)))

(add-hook 'js2-mode-hook 'setup-js2)
(add-hook 'js2-jsx-mode-hook 'setup-js2)

(provide 'my-js)

;;; my-js.el ends here
