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

(defun my-update-cursor ()
  "Change the cursor when god-mode is on."
  (when god-local-mode
    (blink-cursor-mode -1)
    (set-cursor-color "##3b99fc"))
  (unless god-local-mode
    (blink-cursor-mode 1)
    (set-cursor-color "#00ff00")))

(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)

(god-mode-all)

(provide 'my-god)

;;; my-god.el ends here
