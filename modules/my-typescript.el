;;; my-typescript.el --- All about Typescript
;;; Commentary:
;; Configure everything about Typescript

;;; Code:

(require-packages '(typescript-mode tide))

(defun setup-tide-mode ()
  (tide-setup)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1))

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(setq-default typescript-indent-level 2)

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)

(provide 'my-typescript)

;;; my-typescript.el ends here
