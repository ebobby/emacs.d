;;; my-ruby.el --- All about Ruby
;;; Commentary:
;; Configure everything about Ruby

;;; Code:

(require-packages '(rbenv ruby-tools projectile-rails inf-ruby yari))

;; Install Ruby relevant dash docsets
(my-install-official-docset "Ruby_2")
(my-install-official-docset "Ruby_on_Rails_5")

;; Ruby
(require 'ruby-mode)
(push '("Gemfile" . ruby-mode) auto-mode-alist)
(push '("Gemfile.lock" . ruby-mode) auto-mode-alist)
(push '("Rakefile" . ruby-mode) auto-mode-alist)
(push '("Capfile" . ruby-mode) auto-mode-alist)
(push '("\\.rake" . ruby-mode) auto-mode-alist)
(push '("\\.gemspec" . ruby-mode) auto-mode-alist)
(setq ruby-insert-encoding-magic-comment nil)

(define-key 'help-command (kbd "R") 'yari)

;; Ruby Version Manager
(require 'rbenv)
(rbenv-use-global)

;; Ruby tools
(require 'ruby-tools)
(diminish 'ruby-tools-mode)

;; Ruby projects
(add-hook 'projectile-mode-hook 'projectile-rails-on)

(add-hook 'ruby-mode-hook (lambda ()
                            (setq-local helm-dash-docsets '("Ruby" "Ruby on Rails"))
                            (inf-ruby-minor-mode +1)
                            (ruby-tools-mode +1)
                            ;; CamelCase aware editing operations
                            (subword-mode +1)))

(provide 'my-ruby)

;;; my-ruby.el ends here
