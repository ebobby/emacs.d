;;; my-ruby.el --- All about Ruby
;;; Commentary:
;; Configure everything about Ruby

;;; Code:

(require-packages '(rinari rbenv ruby-tools))

;; Ruby
(require 'ruby-mode)
(push '("Gemfile" . ruby-mode) auto-mode-alist)
(push '("Gemfile.lock" . ruby-mode) auto-mode-alist)
(push '("Rakefile" . ruby-mode) auto-mode-alist)
(push '("Capfile" . ruby-mode) auto-mode-alist)
(push '("\\.rake" . ruby-mode) auto-mode-alist)
(push '("\\.gemspec" . ruby-mode) auto-mode-alist)

;; Ruby Version Manager
(require 'rbenv)
(rbenv-use-global)

;; Ruby tools
(require 'ruby-tools)

(provide 'my-ruby)

;;; my-ruby.el ends here
