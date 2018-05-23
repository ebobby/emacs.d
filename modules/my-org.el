x;;; my-org.el --- All about Org
;;; Commentary:
;; Configure everything about org-mode

;;; Code:

(require-packages '(org))

;; Clojure
(require 'org)

(setq org-agenda-files
      (file-expand-wildcards "~/Dropbox/org/*.org"))

(global-set-key "\C-ca" 'org-agenda)

(provide 'my-org)

;;; my-org.el ends here
