;;; my-osx.el --- Configuration for OSX
;;; Commentary:
;; OSX

;;; Code:
;; Turn cmd key into meta
(setq mac-command-modifier 'meta)
(setq ns-function-modifier 'hyper)
(setq mac-option-modifier 'super)

(require-packages '(exec-path-from-shell))

(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; execution path so homebrew binaries work
(setq exec-path (append exec-path '("/usr/local/bin")))
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))

(provide 'my-osx)

;;; my-osx.el ends here
