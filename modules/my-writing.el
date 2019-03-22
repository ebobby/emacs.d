;;; my-writing.el --- All about Clojure
;;; Commentary:
;; Configure everything about Clojure

;;; Code:

(require-packages '(writegood-mode writeroom-mode))

(require 'writegood-mode)
(require 'writeroom-mode)

(add-hook 'text-mode (lambda ()
                       (writegood-mode)
                       (writeroom-mode)))

(define-key my-mode-map (kbd "C-c g") 'writegood-mode)
(define-key my-mode-map (kbd "C-c o") 'writeroom-mode)

(provide 'my-writing)

;;; my-writing.el ends here
