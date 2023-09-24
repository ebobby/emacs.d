;;; my-packages.el --- Package handling.
;; Copyright (C) 2010-2021 Francisco Soto
;; Author: Francisco Soto <ebobby@ebobby.org>
;; URL: https://github.com/ebobby/emacs.d
;;
;; This file is not part of GNU Emacs.
;; This file is free software.
;;; Commentary:
;;; Code:

(require 'package)

;; Initialize packages before handling the rest of the config.
(package-initialize)
;(package-refresh-contents)

(defun require-package (package)
  "Install PACKAGE if not already installed."
  (unless (package-installed-p package)
    (package-install package)))

(defun require-packages (packages)
  "Ensure PACKAGES are installed, install if missing."
  (mapc #'require-package packages))

;; Modes that will be installed automatically when opening these extensions.
;; Used for modes that have no special configuration (yet).
(defvar dumb-modes-alist
  '(("\\.csv\\'" csv-mode csv-mode)
    ("\\.dot\\'" graphviz-dot-mode graphviz-dot-mode)
    ("\\.graphql\\'" graphql-mode graphql-mode)
    ("\\.haml\\'" haml-mode haml-mode)
    ("\\.hbs\\'" handlebars-mode handlebars-mode)
    ("\\.less\\'" less-css-mode less-css-mode)
    ("\\.sass\\'" sass-mode sass-mode)
    ("\\.scss\\'" scss-mode scss-mode)
    ("\\.textile\\'" textile-mode textile-mode)
    ("\\.yaml\\'" yaml-mode yaml-mode)
    ("\\.yml\\'" yaml-mode yaml-mode)))

;;; Blatantly ripped from prelude emacs
(defmacro auto-install (extension package mode)
  "When file with EXTENSION is opened triggers auto-install of PACKAGE.
PACKAGE is installed only if not already present.  The file is opened in MODE."
  `(add-to-list 'auto-mode-alist
                `(,extension . (lambda ()
                                 (unless (package-installed-p ',package)
                                   (package-install ',package))
                                 (,mode)))))

;; build auto-install mappings
(mapc (lambda (entry)
        (let ((extension (car entry))
              (package (cadr entry))
              (mode (cadr (cdr entry))))
          (unless (package-installed-p package)
            (auto-install extension package mode))))
      dumb-modes-alist)

(provide 'my-packages)

;;; my-packages.el ends here
