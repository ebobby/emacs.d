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

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold (* 1024 1024 50))

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; Core configuration
(require 'my-packages)

;; OS specific configuration
(when (equal system-type 'darwin)
  (require 'my-osx))

(require 'my-functions)
(require 'my-editor)
(require 'my-key-bindings)

;; My-mode goes last
(require 'my-mode)

;; Modules configuration
(require 'my-company)
(require 'my-helm)
(require 'my-magit)
(require 'my-lisp)
(require 'my-elisp)
(require 'my-ruby)
(require 'my-js)
(require 'my-web)
(require 'my-rainbow)
(require 'my-haskell)
(require 'my-clojure)
(require 'my-rust)
(require 'my-typescript)
(require 'my-markdown)
(require 'my-writing)
(require 'my-latex)

;; Load UI after everything else.
(require 'my-ui)

(server-start)

;;; init.el ends here
