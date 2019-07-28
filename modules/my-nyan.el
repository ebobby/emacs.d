;;; my-nyan.el --- All about Nyan
;;; Commentary:
;; Configure everything about Nyan

;;; Code:

(require-packages '(nyan-mode zone-nyan))

;; Nyan
(require 'nyan-mode)
(require 'zone-nyan)

(nyan-mode +1)

(provide 'my-nyan)

;;; my-nyan.el ends here
