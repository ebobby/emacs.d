;;; my-clojure.el --- All about Clojure
;;; Commentary:
;; Configure everything about Clojure

;;; Code:

(require-packages '(clojure-mode
                    cider
                    inf-clojure
                    clj-refactor
                    clojure-snippets))

;; Clojure
(require 'clojure-mode)
(require 'clj-refactor)

(add-hook 'clojure-mode-hook (lambda ()
                               (smartparens-strict-mode +1)
                               (clj-refactor-mode +1)
                               (clj-add-keybindings-with-prefix "C-c C-m")))

(eval-after-load 'cider
  '(progn
     (setq nrepl-log-messages t)
     (add-hook 'cider-mode-hook 'eldoc-mode)))


(provide 'my-clojure)

;;; my-clojure.el ends here
