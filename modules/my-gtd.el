;;; my-gtd.el --- All about GTD
;;; Commentary:
;; Configure everything about GTD

;;; Code:

(require-packages '(org))

;; org-mode for GTD
(require 'org)

(setq org-agenda-files
      (list "~/Dropbox/gtd/inbox.org"
            "~/Dropbox/gtd/carezone.org"
            "~/Dropbox/gtd/projects.org"
            "~/Dropbox/gtd/tickler.org"))

(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/Dropbox/gtd/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "~/Dropbox/gtd/tickler.org" "Tickler")
                               "* %i%? \n %U")))

(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

(provide 'my-gtd)

;;; my-gdt.el ends here
