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
                               (clj-refactor-mode 1)
                               (inf-clojure-minor-mode 1)
                               (smartparens-strict-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c C-h")))

(eval-after-load 'cider
  '(progn
     (setq nrepl-log-messages t)
     (add-hook 'cider-mode-hook (lambda ()
                                  (eldoc-mode 1)
                                  (inf-clojure-minor-mode -1)))))


(provide 'my-clojure)

;;; my-clojure.el ends here
