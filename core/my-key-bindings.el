;;; my-key-bindings.el --- Global bindings
;;; Commentary:
;; Key bindings

;;; Code:

(defvar my-bindings-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-c") 'confirm-exit-emacs)
    (define-key map (kbd "<f1>") 'shell)
    (define-key map (kbd "<f2>") 'rgrep)
    (define-key map (kbd "<f7>") 'toggle-truncate-lines)
    (define-key map (kbd "<f10>") 'magit-status)
    (define-key map (kbd "<f11>") 'toggle-frame-fullscreen)
    (define-key map (kbd "M-1") 'delete-other-windows)
    (define-key map (kbd "M-2") 'split-window-vertically)
    (define-key map (kbd "M-3") 'split-window-horizontally)
    (define-key map (kbd "M-0") 'delete-window)
    (define-key map (kbd "M-o") 'ace-window)
    (define-key map (kbd "M-k") 'kill-this-buffer)
    (define-key map (kbd "C-x C-m") 'smex)
    (define-key map (kbd "M-x") 'smex-major-mode-commands)
    (define-key map (kbd "C-c C-p C-d") 'dash-at-point)
    (define-key map (kbd "C-z") 'repeat)
    (define-key map (kbd "C-x C-r") 'recentf-open-files)
    (define-key map (kbd "C-<return>") 'isearch-forward-symbol-at-point)
    (define-key map (kbd "C-x C-b") 'ibuffer)
    (define-key map (kbd "C-=") 'er/expand-region)
    (define-key map (kbd "C-c j") 'ace-jump-mode)
    (define-key map (kbd "s-y") 'browse-kill-ring)
    (define-key map (kbd "C-c p f") 'projectile-find-file)
    (define-key map (kbd "C-c m") 'discover-my-major)
    (define-key map (kbd "C-|") 'hippie-expand)
    map))

(define-minor-mode my-key-bindings-minor-mode
  "A minor mode that sets my own key bindings globally." t " My" my-bindings-map)

;; Unset a bunch of keys
(global-unset-key (kbd "C-x C-c"))
(global-unset-key (kbd "C-x 1"))
(global-unset-key (kbd "C-x 2"))
(global-unset-key (kbd "C-x 3"))
(global-unset-key (kbd "C-x 0"))
(global-unset-key (kbd "C-x o"))
(global-unset-key (kbd "C-x o"))
(global-unset-key (kbd "C-x k"))

(my-key-bindings-minor-mode +1)

(defun turn-off-bindings-setup-hook ()
  "Turn the my-keys mode off."
  (my-key-bindings-minor-mode 0))

(add-hook 'minibuffer-setup-hook 'turn-off-bindings-setup-hook)

;; Make sure this mode has preference over all modes.
(defadvice load (after give-my-keybindings-priority)
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'my-key-bindings-minor-mode))
      (let ((mykeys (assq 'my-key-bindings-minor-mode minor-mode-map-alist)))
        (assq-delete-all 'my-key-bindings-minor-mode minor-mode-map-alist)
        (push mykeys minor-mode-map-alist))))

(ad-activate 'load)

(provide 'my-key-bindings)

;;; my-key-bindings.el ends here
