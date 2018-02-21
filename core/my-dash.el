;;; my-dash.el --- Dash docsets misc configuration
;;; Commentary:
;; Docs.

;;; Code:

;; Global docsets
(my-install-official-docset "PostgreSQL")
(my-install-official-docset "Redis")
(my-install-official-docset "Nginx")

(setq helm-dash-common-docsets '("PostgreSQL"
                                 "Redis"
                                 "Nginx"))
(provide 'my-dash)

;;; my-dash.el ends here
