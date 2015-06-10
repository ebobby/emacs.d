;;; my-rust.el --- All about Rust
;;; Commentary:
;; Configure everything about Rust

;;; Code:

(require-packages '(rust-mode flycheck-rust))

;; Rust mode
(require 'rust-mode)

(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(provide 'my-rust)

;;; my-rust.el ends here
