;;; my-ruby.el --- All about Ruby  -*- lexical-binding: t; -*-
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
         ("/Gemfile\\'" . ruby-mode)
         ("/Rakefile\\'" . ruby-mode)
         ("/Capfile\\'" . ruby-mode))
  :config
  (setq lsp-solargraph-use-bundler t
        ruby-insert-encoding-magic-comment nil)

  ;; Use solargraph: rubocop's and typeprof's servers would otherwise compete.
  (with-eval-after-load 'lsp-mode
    (dolist (client '(rubocop-ls typeprof-ls))
      (add-to-list 'lsp-disabled-clients client))))

(provide 'my-ruby)

;;; my-ruby.el ends here
