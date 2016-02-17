;;; my-packages.el --- Package handling.
;;; Commentary:
;; Package handling.

;;; Code:

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(defvar my-packages '(ace-window
                      anzu
                      diff-hl
                      diminish
                      discover-my-major
                      epl
                      expand-region
                      flycheck
                      iedit
                      smartparens
                      nlinum
                      org
                      powerline
                      projectile
                      rainbow-delimiters
                      volatile-highlights
                      yasnippet
                      moe-theme)

  "A list of required packages to ensure they are installed at launch.")

(defun require-package (package)
  "Install PACKAGE if not already installed."
  (unless (member package my-packages)
    (push package my-packages))
  (unless (package-installed-p package)
    (package-install package)))

(defun require-packages (packages)
  "Ensure PACKAGES are installed, install if missing."
  (mapc #'require-package packages))

(defun install-my-packages ()
  "Install all pacjages in `my-packages'."
  (unless (every #'package-installed-p my-packages)
    (package-refresh-contents)
    (require-packages my-packages)))

(defun list-extra-packages ()
  "List all installed packages not defined in `my-packages'."
  (interactive)
  (package-show-package-list (set-difference package-activated-list
                                             my-packages)))

(install-my-packages)
(require 'epl)

(provide 'my-packages)

;;; my-packages.el ends here
