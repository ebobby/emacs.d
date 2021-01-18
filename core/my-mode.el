;;; my-mode.el --- Global bindings
;;; Commentary:
;; Key bindings

;;; Code:

(global-unset-key  (kbd "s-m"))

(defvar my-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<f7>") 'toggle-truncate-lines)
    (define-key map (kbd "<f8>") 'my-neotree-project)
    (define-key map (kbd "C-;") 'mc/mark-all-symbols-like-this)
    (define-key map (kbd "C-c t") 'vr/replace)
    (define-key map (kbd "C-c q") 'vr/query-replace)
    (define-key map (kbd "C-x m a") 'mc/edit-beginnings-of-lines)
    (define-key map (kbd "C-x m e") 'mc/edit-ends-of-lines)
    (define-key map (kbd "C-x m l") 'mc/edit-lines)
    (define-key map (kbd "C-x m p") 'move-text-up)
    (define-key map (kbd "C-x m n") 'move-text-down)
    (define-key map (kbd "<s-backspace>") 'backward-kill-word)
    (define-key map (kbd "C-z") 'repeat)
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

;; Make sure this mode has preference over all modes.
(defadvice load (after give-my-keybindings-priority)
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'my-mode))
      (let ((mykeys (assq 'my-mode minor-mode-map-alist)))
        (assq-delete-all 'my-mode minor-mode-map-alist)
        (push mykeys minor-mode-map-alist))))

(ad-activate 'load)

(my-global-mode +1)

(provide 'my-mode)

;;; my-mode.el ends here
