;;; my-settings.el --- Default settings.
;; Copyright (C) 2010-2021 Francisco Soto
;; Author: Francisco Soto <ebobby@ebobby.org>
;; URL: https://github.com/ebobby/emacs.d
;;
;; This file is not part of GNU Emacs.
;; This file is free software.
;;; Commentary:
;;; Code:

(setq
 ;; User directory.
 user-emacs-directory user-dir

 ;; Reduce *Message* noise at startup.
 inhibit-default-init t
 inhibit-startup-echo-area-message user-login-name
 inhibit-startup-message t
 initial-major-mode 'text-mode

 ;; Welcome message.
 initial-scratch-message nil

 ;; OSX keybindings.
 mac-command-modifier 'meta
 mac-option-modifier 'super
 ns-function-modifier 'hyper

 ;; Packages config.
 gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"
 package-archives '(("org"          . "https://orgmode.org/elpa/")
                    ("melpa"        . "https://melpa.org/packages/")
                    ("gnu"          . "https://elpa.gnu.org/packages/"))

 ;; tabs and indentation
 require-final-newline t
 standard-indent 2

 ;; store all backup and autosave files in the tmp dir
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
 auto-save-list-file-prefix temporary-file-directory
 backup-directory-alist `(("." . ,backup-dir))

 ;; Just in time lock
 jit-lock-defer-time nil
 jit-lock-stealth-nice 0.1
 jit-lock-stealth-time 0.2
 jit-lock-stealth-verbose nil

 ;; Stop emacs from asking dumb questions.
 confirm-kill-processes nil
 confirm-nonexistent-file-or-buffer nil
 kill-buffer-query-functions (remq 'process-kill-buffer-query-function
                                   kill-buffer-query-functions)
 ;; PDF viewing
 doc-view-continuous t
 doc-view-resolution 300

 ;; UI
 blink-matching-paren nil
 ns-pop-up-frames nil
 query-replace-highlight t
 ring-bell-function 'ignore
 scroll-conservatively 100000
 scroll-margin 5
 scroll-preserve-screen-position 1
 search-highlight t
 ;;suggest-key-bindings nil
 transient-mark-mode t

 ;; Hippie Expand
 hippie-expand-try-functions-list '(try-expand-dabbrev
                                    try-expand-dabbrev-all-buffers
                                    try-expand-dabbrev-from-kill
                                    try-complete-file-name-partially
                                    try-complete-file-name
                                    try-expand-all-abbrevs
                                    try-expand-list
                                    try-expand-line
                                    try-complete-lisp-symbol-partially
                                    try-complete-lisp-symbol)
 ;; Unique buffer names.
 uniquify-after-kill-buffer-p t
 uniquify-buffer-name-style 'forward
 uniquify-ignore-buffers-re "^\\*"
 uniquify-separator "/"

 ;; Misc.
 apropos-do-all t
 bookmark-default-file (expand-file-name "bookmarks" savefile-dir)
 create-lockfiles nil
 dired-listing-switches "-alh"
 echo-keystrokes 0.1
 large-file-warning-threshold (* 1024 1024 100 1)
 read-process-output-max (* 1024 1024)
 search-default-mode #'char-fold-to-regexp)
;;;;;;;;

;; encoding
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; Do not ask about running processes when exiting.
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-flet ((process-list ())) ad-do-it))

;;; Normally disabled commands
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(setq-default indent-tabs-mode nil
              tab-width 2
              truncate-lines t
              warning-minimum-level :emergency
              fill-column 80

              ;; Use aspell instead of ispell
              ispell-program-name "aspell"
              ispell-extra-args '("--sug-mode=ultra")

              ;; Long lines fixes
              bidi-paragraph-direction 'left-to-right
              bidi-inhibit-bpa t)

;; Anwsering y/n is faster than yes/no.
(fset 'yes-or-no-p 'y-or-n-p)

;; apply syntax highlighting to all buffers
(global-font-lock-mode t)

;; show column number in bar
(column-number-mode t)

;; highlight matching parens
(show-paren-mode t)

;; blink cursor
(blink-cursor-mode -1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Revert buffers that change externally
(global-auto-revert-mode t)

;; Remove whitespace on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; sql-postgres prompt change, default mishandles dashes.
;;(sql-set-product-feature 'postgres :prompt-regexp "^[[:alnum:]_-]*=[#>]")

;; Save sessions
(desktop-save-mode t)

(provide 'my-settings)

;;; my-settings.el ends here
