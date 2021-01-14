;;; my-rust.el --- All about Rust
;;; Commentary:
;; Configure everything about Rust

;;; Code:

(require-packages '(rust-mode racer flycheck-rust cargo))

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(exec-path-from-shell-copy-env "RUST_SRC_PATH")

(eval-after-load 'rust-mode
  '(progn
     (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)
     (add-hook 'racer-mode-hook 'eldoc-mode)
     (add-hook 'rust-mode-hook (lambda ()
                                 (subword-mode)
                                 (cargo-minor-mode)
                                 (racer-mode)
                                 (local-set-key (kbd "C-c C-d") 'racer-describe)
                                 (lsp)
                                 (diminish 'cargo-minor-mode)
                                 (diminish 'racer-mode)))))

(provide 'my-rust)

;;; my-rust.el ends here
