;;; my-octave.el --- All about Octave
;;; Commentary:
;; Configure everything about Octave

;;; Code:

(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))

(provide 'my-octave)

;;; my-octave.el ends here
