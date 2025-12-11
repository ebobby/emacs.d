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

(use-package ruby-ts-mode
  :hook ((ruby-ts-mode . lsp-deferred)
         (ruby-ts-mode . inf-ruby-minor-mode))
  :mode (("\\.rb\\'"  . ruby-ts-mode)
         ("\\.rake\\'" . ruby-ts-mode)
         ("\\.gemspec\\'" . ruby-ts-mode)
         ("Gemfile" . ruby-ts-mode)
         ("Gemfile.lock" . ruby-ts-mode)
         ("Rakefile" . ruby-ts-mode)
         ("Capfile" . ruby-ts-mode))
  :config
  (require 'lsp-solargraph)
  (setq lsp-solargraph-use-bundler t
        ruby-insert-encoding-magic-comment nil)

  ;; Prioritize ruby-lsp over solargraph.
  (let ((ruby-lsp (gethash 'ruby-ls lsp-clients)))
    (when ruby-lsp (setf (lsp--client-priority ruby-lsp) 1))))

(provide 'my-ruby)

;;; my-ruby.el ends here
