;;; my-packages.el --- Package handling.
;;; Commentary:
;; Package handling.

;;; Code:

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(defvar my-packages '(ace-jump-buffer
                      ace-jump-mode
                      ace-window
                      anzu
                      browse-kill-ring
                      dash-at-point
                      diff-hl
                      diminish
                      dired+
                      discover-my-major
                      epl
                      exec-path-from-shell
                      expand-region
                      flycheck
                      ido-ubiquitous
                      iedit
                      move-text
                      nlinum
                      org
                      powerline
                      projectile
                      rainbow-delimiters
                      rust-mode
                      smex
                      volatile-highlights
                      yasnippet
                      writegood-mode

                      color-theme-sanityinc-tomorrow
                      leuven-theme
                      moe-theme
                      monokai-theme
                      solarized-theme
                      zenburn-theme)
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
