;;; my-packages.el --- Package handling.
;;; Commentary:
;; Package handling.

;;; Code:

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(defvar my-packages '(ace-window
                      all-the-icons
                      anzu
                      diff-hl
                      diminish
                      discover-my-major
                      doom-modeline
                      doom-themes
                      dumb-jump
                      epl
                      exec-path-from-shell
                      expand-region
                      flycheck
                      flycheck-pos-tip
                      helm
                      helm-ag
                      helm-c-yasnippet
                      helm-descbinds
                      helm-flycheck
                      helm-ls-git
                      iedit
                      imenu-list
                      move-text
                      neotree
                      org
                      smartparens
                      visual-regexp
                      volatile-highlights
                      yasnippet
                      yasnippet-snippets)
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

;;; Blatantly ripped from prelude emacs
(defmacro auto-install (extension package mode)
  "When file with EXTENSION is opened triggers auto-install of PACKAGE.
PACKAGE is installed only if not already present.  The file is opened in MODE."
  `(add-to-list 'auto-mode-alist
                `(,extension . (lambda ()
                                 (unless (package-installed-p ',package)
                                   (package-install ',package))
                                 (,mode)))))

(defvar auto-install-alist
  '(("\\.coffee\\'" coffee-mode coffee-mode)
    ("\\.css\\'" css-mode css-mode)
    ("\\.csv\\'" csv-mode csv-mode)
    ("\\.elm\\'" elm-mode elm-mode)
    ("\\.ex\\'" elixir-mode elixir-mode)
    ("\\.exs\\'" elixir-mode elixir-mode)
    ("\\.elixir\\'" elixir-mode elixir-mode)
    ("\\.erl\\'" erlang erlang-mode)
    ("\\.go\\'" go-mode go-mode)
    ("\\.haml\\'" haml-mode haml-mode)
    ("\\.hbs\\'" handlebars-mode handlebars-mode)
    ("\\.json\\'" json-mode json-mode)
    ("\\.latex\\'" auctex LaTeX-mode)
    ("\\.less\\'" less-css-mode less-css-mode)
    ("\\.lua\\'" lua-mode lua-mode)
    ("\\.md\\'" markdown-mode markdown-mode)
    ("\\.php\\'" php-mode php-mode)
    ("\\.rs\\'" rust-mode rust-mode)
    ("\\.sass\\'" sass-mode sass-mode)
    ("\\.scala\\'" scala-mode2 scala-mode)
    ("\\.scss\\'" scss-mode scss-mode)
    ("\\.slim\\'" slim-mode slim-mode)
    ("\\.swift\\'" swift-mode swift-mode)
    ("\\.textile\\'" textile-mode textile-mode)
    ("\\.yml\\'" yaml-mode yaml-mode)
    ("\\.yaml\\'" yaml-mode yaml-mode)))

;; build auto-install mappings
(mapc
 (lambda (entry)
   (let ((extension (car entry))
         (package (cadr entry))
         (mode (cadr (cdr entry))))
     (unless (package-installed-p package)
       (auto-install extension package mode))))
 auto-install-alist)

(provide 'my-packages)

;;; my-packages.el ends here
