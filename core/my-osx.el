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
(exec-path-from-shell-initialize)

(provide 'my-osx)

;;; my-osx.el ends here
