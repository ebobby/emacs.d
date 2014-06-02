;;; init.el --- Emacs configuration
;;; Commentary:
;; Load everything up.

;;; Code:

(require 'cl)
(require 'package)

;; Always load newest byte code
(setq load-prefer-newer t)

(defvar root-dir (file-name-directory load-file-name))
(defvar core-dir (expand-file-name "core" root-dir))
(defvar modes-dir (expand-file-name  "modes" root-dir))
(defvar vendor-dir (expand-file-name "vendor" root-dir))
(defvar savefile-dir (expand-file-name "savefile" root-dir))

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
(add-to-list 'load-path modes-dir)
(add-to-list 'load-path vendor-dir)
(add-to-list 'load-path core-dir)
(add-subfolders-to-load-path vendor-dir)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; Core configuration
(require 'my-packages)
(require 'my-editor)
(require 'my-ui)
(require 'my-bindings)
(when (equal system-type 'darwin)
  (require 'my-osx))

;; Modes configuration
(require 'my-ido)
(require 'my-magit)
(require 'my-ruby)
(require 'my-js)
(require 'my-lisp)
(require 'my-text)
(require 'my-rainbow)
(require 'my-nxhtml)

;;; init.el ends here
