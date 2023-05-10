;;; init.el --- Emacs configuration
;;; Commentary:

;; Load everything up.

;;; Code:
(require 'cl)
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

;; Turn off mouse interface early in startup to avoid momentary display.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Native compilation.
(setq package-native-compile t)
(setq native-comp-compiler-options '("-O2" "-mtune=znver3" "-march=znver3"))

;; Core configuration
(require 'my-settings)
(require 'my-functions)
(require 'my-packages)
(require 'my-editor)
(require 'my-keys)

;; Modules configuration
(require 'my-elisp)
(require 'my-js)
(require 'my-ts)
(require 'my-python)
(require 'my-rust)
(require 'my-latex)
(require 'my-ruby)
(require 'my-haskell)
(require 'my-web)

;; Load UI after everything else.
(require 'my-theme)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
  )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
