;;; my-god.el --- God
;;; Commentary:
;; god-mode configuration

;;; Code:

(require-packages '(god-mode))

(require 'god-mode)
(require 'god-mode-isearch)

(global-set-key (kbd "<escape>") 'god-mode-all)

(define-key isearch-mode-map (kbd "<escape>") 'god-mode-isearch-activate)
(define-key god-mode-isearch-map (kbd "<escape>") 'god-mode-isearch-disable)

(define-key god-local-mode-map (kbd "z") 'repeat)
(define-key god-local-mode-map (kbd "i") 'god-local-mode)

(god-mode-all)

(provide 'my-god)

;;; my-god.el ends here
