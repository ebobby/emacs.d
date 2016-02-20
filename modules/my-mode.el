;;; my-mode.el --- Global bindings
;;; Commentary:
;; Key bindings

;;; Code:

(global-unset-key  (kbd "s-m"))

(defvar my-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<f7>") 'toggle-truncate-lines)
    (define-key map (kbd "C-c r") 'vr/replace)
    (define-key map (kbd "C-c q") 'vr/query-replace)
    (define-key map (kbd "C-x m p") 'move-text-up)
    (define-key map (kbd "C-x m n") 'move-text-down)
    map))

;; define minor mode
(define-minor-mode my-mode
  "A minor mode that sets my own key bindings globally."
  :lighter " my"
  :keymap my-mode-map)

(define-globalized-minor-mode my-global-mode my-mode my-on)

(defun my-on ()
  "Turn on `my-mode'."
  (my-mode +1))

(my-global-mode +1)

(diminish 'my-mode)

(provide 'my-mode)

;;; my-mode.el ends here
