;;; my-helm.el --- Helm
;;; Commentary:
;; Helm configuration

;;; Code:
(require 'helm)
(require 'helm-config)
(require 'helm-ls-git)

(when (executable-find "curl")
  (setq helm-net-prefer-curl t))

;; See https://github.com/bbatsov/prelude/pull/670 for a detailed
;; discussion of these options.
(setq helm-split-window-in-side-p           t
      helm-mode-fuzzy-match                 t
      helm-completion-in-region-fuzzy-match t
      helm-recentf-fuzzy-match              t
      helm-buffers-fuzzy-matching           t
      helm-locate-fuzzy-match               t
      helm-M-x-fuzzy-match                  t
      helm-semantic-fuzzy-match             t
      helm-imenu-fuzzy-match                t
      helm-apropos-fuzzy-match              t
      helm-lisp-fuzzy-completion            t
      helm-ff-fuzzy-matching                t
      helm-move-to-line-cycle-in-source     t
      helm-ff-search-library-in-sexp        t
      helm-ff-file-name-history-use-recentf t)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)

(global-set-key (kbd "C-h f") 'helm-apropos)
(global-set-key (kbd "C-h r") 'helm-info-emacs)
(global-set-key (kbd "C-h y") 'helm-yas-complete)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-h i") 'helm-imenu)
(global-set-key (kbd "C-h C-l") 'helm-locate-library)
(global-set-key (kbd "C-h C-r") 'helm-recentf)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-m") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; Projects
(global-set-key (kbd "C-c p f") 'helm-browse-project)
(global-set-key (kbd "C-c p k") 'helm-project-kill-buffers)

;; Searching
(define-key my-mode-map (kbd "<f2>") 'helm-ag-project-root)
(define-key my-mode-map (kbd "<f3>") 'helm-do-ag-project-root)
(define-key my-mode-map (kbd "<f4>") 'helm-occur)

(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

;; shell history.
(define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z") 'helm-select-action) ; list actions using C-z

(substitute-key-definition 'find-tag 'helm-etags-select global-map)
(helm-descbinds-mode)
(helm-mode 1)

;; Configuration
(setq helm-exit-idle-delay 0) ; If this is higher helm can't keep up with my typing.

(diminish 'helm-mode)

(provide 'my-helm)

;;; my-helm.el ends here
