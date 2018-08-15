;;; my-rust.el --- All about Rust
;;; Commentary:
;; Configure everything about Rust

;;; Code:

(require-packages '(rust-mode racer flycheck-rust))

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(setq company-tooltip-align-annotations t)

(provide 'my-rust)

;;; my-rust.el ends here
