;;; my-clojure.el --- All about Clojure
;;; Commentary:
;; Configure everything about Clojure

;;; Code:

(require-packages '(clojure-mode cider flycheck-clojure))

;; Clojure
 (require 'clojure-mode)

(add-hook 'clojure-mode-hook (lambda ()
                               (smartparens-strict-mode +1)))

(eval-after-load 'flycheck '(flycheck-clojure-setup))

(eval-after-load 'cider
  '(progn
     (setq nrepl-log-messages t)
     (add-hook 'cider-mode-hook 'eldoc-mode)))

(provide 'my-clojure)

;;; my-clojure.el ends here
