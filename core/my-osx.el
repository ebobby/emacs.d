;;; my-osx.el --- Configuration for OSX
;;; Commentary:
;; OSX

;;; Code:

(load-file "/Applications/Emacs.app/Contents/Resources/lisp/shell.el.gz")
(setq explicit-shell-file-name "/usr/local/bin/bash")

;; Turn cmd key into meta
(setq mac-command-modifier 'meta)

(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


(add-hook 'inferior-js-mode-hook
          (lambda ()
            ;; We like nice colors
            (ansi-color-for-comint-mode-on)
            ;; Deal with some prompt nonsense
            (push (lambda (output)
                    (replace-regexp-in-string "\033\\[[0-9]+[GKJ]" "" output))
                  comint-preoutput-filter-functions)))

(provide 'my-osx)

;;; my-osx.el ends here
