;;; my-js.el --- All about JS
;;; Commentary:
;; Configure everything about JS

;;; Code:

(require-packages '(js2-mode js-comint))

(require 'js2-mode)
(autoload 'js2-mode "js" nil t)
(push '("\\.js$" . js2-mode) auto-mode-alist)
(push '("\\.json$" . js2-mode) auto-mode-alist)
(add-hook 'js2-mode-hook '(lambda ()
                            (local-set-key "\C-x\C-e" 'js-send-last-sexp)
                            (local-set-key "\C-cb" 'js-send-buffer)
                            (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)))
(setq-default js2-basic-offset 2)

(require 'js-comint)
(setq inferior-js-program-command "node --")

(provide 'my-js)

;;; my-js.el ends here
