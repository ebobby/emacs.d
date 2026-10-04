;;; init.el --- Emacs configuration  -*- lexical-binding: t; -*-
;;; Commentary:

;; Load everything up.

;;; Code:
(require 'cl-lib)
(require 'package)

;; Define global directories.
(defvar root-dir (file-name-directory load-file-name))
(defvar backup-dir (expand-file-name "backup" root-dir))
(defvar savefile-dir (expand-file-name "savefile" root-dir))
(defvar user-dir (expand-file-name "user-files" root-dir))
(defvar utilities-dir (expand-file-name "utilities" root-dir))

;; Our configuration.
(add-to-list 'load-path (expand-file-name "core" root-dir))
(add-to-list 'load-path (expand-file-name "modules" root-dir))

;; Keep Customize's writes out of this file.
(setq custom-file (expand-file-name "custom.el" user-dir))

;; Native compilation.
(when (string-match "NATIVE_COMP" system-configuration-features)
  (setq package-native-compile t))

;; Core configuration
(require 'my-settings)
(require 'my-functions)
(require 'my-packages)
(require 'my-editor)
(require 'my-keys)

;; Modules configuration
(require 'my-ai)
(require 'my-elisp)
(require 'my-go)
(require 'my-haskell)
(require 'my-js)
(require 'my-python)
(require 'my-ruby)
(require 'my-rust)
(require 'my-web)
(require 'my-ios)
(require 'my-android)
(require 'my-writing)

;; Load UI after everything else.
(require 'my-ui)

(load custom-file t)

;;; init.el ends here
