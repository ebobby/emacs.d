;;; init.el --- Emacs configuration
;;; Commentary:

(package-initialize)

;; Load everything up.

;;; Code:

(require 'cl)
(require 'package)

;; Always load newest byte code
(setq load-prefer-newer t)

(defvar root-dir (file-name-directory load-file-name))
(defvar core-dir (expand-file-name "core" root-dir))
(defvar modules-dir (expand-file-name  "modules" root-dir))
(defvar vendor-dir (expand-file-name "vendor" root-dir))
(defvar savefile-dir (expand-file-name "savefile" root-dir))
(defvar backup-dir (expand-file-name "backup" root-dir))
(defvar utilities-dir (expand-file-name "utilities" root-dir))

(setq user-emacs-directory (expand-file-name "user-files" root-dir ))

;; Copied from prelude.
(defun add-subfolders-to-load-path (parent-dir)
  "Add all level PARENT-DIR subdirs to the `load-path'."
  (dolist (f (directory-files parent-dir))
    (let ((name (expand-file-name f parent-dir)))
      (when (and (file-directory-p name)
                 (not (string-prefix-p "." f)))
        (add-to-list 'load-path name)
        (add-subfolders-to-load-path name)))))

(add-to-list 'load-path core-dir)
(add-to-list 'load-path modules-dir)
(add-to-list 'load-path vendor-dir)
(add-to-list 'load-path core-dir)
(add-subfolders-to-load-path vendor-dir)

;; Garbage collection configuration
(setq gc-cons-threshold 100000000)
(setq gc-cons-percentage 0.5)
(setq read-process-output-max (* 1024 1024))

(run-with-idle-timer 5 t #'garbage-collect)
(setq garbage-collection-messages t)

;; warn when opening files bigger than 200MB
(setq large-file-warning-threshold (* 1024 1024 100 2))

;; Core configuration
(require 'my-packages)

;; OS specific configuration
(when (equal system-type 'darwin)
  (require 'my-osx))

;; Core configuration
(require 'my-functions)
(require 'my-editor)
(require 'my-key-bindings)
(require 'my-mode)
(require 'my-company)
(require 'my-helm)
(require 'my-magit)
(require 'my-rainbow)
(require 'my-lsp)

;; Modules configuration
(require 'my-clojure)
(require 'my-elisp)
(require 'my-haskell)
(require 'my-js)
(require 'my-latex)
(require 'my-lisp)
(require 'my-markdown)
(require 'my-ruby)
(require 'my-python)
(require 'my-rust)
(require 'my-typescript)
(require 'my-web)
(require 'my-octave)

;; Load UI after everything else.
(require 'my-ui)

(server-start)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (posframe transient treemacs clojure-mode lsp-mode yasnippet-snippets yari yaml-mode which-key web-mode volatile-highlights visual-regexp tide smartparens slime scss-mode ruby-tools rbenv rainbow-mode rainbow-identifiers rainbow-delimiters racer pyenv-mode py-autopep8 prettier-js nvm neotree move-text moe-theme magit lsp-ui latex-preview-pane js2-mode js-comint intero inf-ruby inf-clojure imenu-list helm-lsp helm-ls-git helm-flycheck helm-descbinds helm-c-yasnippet helm-bibtex helm-ag haskell-snippets flycheck-rust flycheck-pos-tip expand-region exec-path-from-shell elpy dumb-jump doom-themes doom-modeline discover-my-major diminish diff-hl dap-mode csv-mode company-auctex common-lisp-snippets clojure-snippets clj-refactor cargo blacken anzu)))
 '(safe-local-variable-values
   (quote
    ((org-todo-keyword-faces
      ("CODE" . "DeepPink")
      ("TODO" . "DeepPink")
      ("MEETING" . "pink")
      ("DONE" . "OliveDrab")
      ("ATTENDED" . "OliveDrab")
      ("MERGED" . "ForestGreen")
      ("CANCELED" . "red")
      ("STARTED" . "LightCoral")
      ("COMMITTED" . "firebrick")
      ("HOLD" . "LightGoldenrod"))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
