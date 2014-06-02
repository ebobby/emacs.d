;;; my-nxhtml.el --- nxhtml
;;; Commentary:
;; nxhtml

;;; Code:

(load (expand-file-name "autostart.el"
                        (expand-file-name "nxhtml" vendor-dir)))

;; nxhtml
(setq nxhtml-global-minor-mode t
      mumamo-chunk-coloring 'submode-colored
      nxhtml-skip-welcome t
      indent-region-mode t
      rng-nxml-auto-validate-flag nil
      nxml-degraded t)

(provide 'my-nxhtml)

;;; my-nxhtml.el ends here
