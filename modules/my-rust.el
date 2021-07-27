;;; my-rust.el --- All about Rust
;; Copyright (C) 2010-2021 Francisco Soto
;; Author: Francisco Soto <ebobby@ebobby.org>
;; URL: https://github.com/ebobby/emacs.d
;;
;; This file is not part of GNU Emacs.
;; This file is free software.
;;; Commentary:
;;; Code:

(use-package flycheck-rust
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package cargo)

(use-package rust-mode
  :hook ((rust-mode . lsp)
         (rust-mode . cargo-minor-mode)
         (rust-mode . subword-mode))
  :mode "\\.rs\\'"
  :bind (:map rust-mode-map
              ("C-c C-d" . racer-describe))
  :config
  (exec-path-from-shell-copy-env "RUST_SRC_PATH"))

(provide 'my-rust)

;;; my-rust.el ends here
