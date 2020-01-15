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
(setq gc-cons-threshold (* 511 1024 1024))
(setq gc-cons-percentage 0.5)
(run-with-idle-timer 5 t #'garbage-collect)
(setq garbage-collection-messages t)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("855eb24c0ea67e3b64d5d07730b96908bac6f4cd1e5a5986493cbac45e9d9636" default)))
 '(package-selected-packages
   (quote
    (memoize lsp-java treemacs company-lsp helm-lsp dap-mode lsp-ui lsp-mode dash markdown-mode shrink-path yasnippet git-commit sesman yasnippet-snippets typescript-mode tide parseclj json-mode yaml-mode hydra helm-core avy transient with-editor writeroom-mode writegood-mode cargo flycheck-rust racer rust-mode clojure-snippets clj-refactor inf-clojure cider clojure-mode haskell-snippets intero haskell-mode rainbow-identifiers rainbow-delimiters rainbow-mode web-mode nvm js-comint company-tern tern js2-mode yari inf-ruby ruby-tools rbenv elisp-slime-nav common-lisp-snippets slime magit company which-key volatile-highlights visual-regexp smartparens neotree multiple-cursors move-text imenu-list helm-ls-git helm-flycheck helm-descbinds helm-ag helm flycheck-pos-tip flycheck expand-region exec-path-from-shell epl dumb-jump doom-themes doom-modeline discover-my-major diminish diff-hl anzu all-the-icons ace-window)))
 '(safe-local-variable-values
   (quote
    ((org-todo-keyword-faces
      ("CODE" . "DeepPink")
      ("TODO" . "DeepPink")
      ("MEETING" . "pink")
      ("DONE" . "OliveDrab")
      ("ATTENDED" . "OliveDrab")
      ("MERGED" . "ForestGreen")
      ("CANCELED" . "brown")
      ("STARTED" . "LightCoral")
      ("COMMITTED" . "firebrick"))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


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

;; Modules configuration
(require 'my-clojure)
(require 'my-elisp)
(require 'my-haskell)
(require 'my-js)
(require 'my-latex)
(require 'my-lisp)
(require 'my-markdown)
(require 'my-ruby)
(require 'my-rust)
(require 'my-typescript)
(require 'my-web)

;; Load UI after everything else.
(require 'my-nyan)
(require 'my-ui)

(server-start)

;;; init.el ends here
