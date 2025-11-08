;;; my-ruby.el --- All about Ruby
;; Copyright (C) 2010-2021 Francisco Soto
;; Author: Francisco Soto <ebobby@ebobby.org>
;; URL: https://github.com/ebobby/emacs.d
;;
;; This file is not part of GNU Emacs.
;; This file is free software.
;;; Commentary:
;;; Code:

(use-package inf-ruby)

(use-package ruby-mode
  :hook ((ruby-mode . lsp-deferred)
         (ruby-mode . inf-ruby-minor-mode))
  :mode (("\\.rb\\'"  . ruby-mode)
         ("\\.rake\\'" . ruby-mode)
         ("\\.gemspec\\'" . ruby-mode)
         ("Gemfile" . ruby-mode)
         ("Gemfile.lock" . ruby-mode)
         ("Rakefile" . ruby-mode)
         ("Capfile" . ruby-mode))
  :config
  (require 'lsp-ruby-lsp)
  (require 'lsp-solargraph)
  (setq ruby-insert-encoding-magic-comment nil)

  (let ((ruby-lsp-ls (gethash 'ruby-lsp-ls lsp-clients))
        (solargraph (gethash 'ruby-ls lsp-clients)))
    (when solargraph (setf (lsp--client-priority solargraph) 1))
    (when ruby-lsp-ls (setf (lsp--client-priority ruby-lsp-ls) 2))))

(provide 'my-ruby)

;;; my-ruby.el ends here
