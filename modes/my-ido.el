;;; my-ido.el --- Configure ido
;;; Commentary:
;; Ido

;;; Code:

(require-packages '(flx-ido ido-ubiquitous smex ido-vertical-mode))

(require 'ido)
(require 'ido-ubiquitous)
(require 'flx-ido)
(require 'ido-vertical-mode)
(require 'smex)

(push "\\.DS_Store" ido-ignore-files)
(push "\\.Trash" ido-ignore-files)

(setq ido-enable-prefx nil
      ido-everywhere t
      ido-enable-flex-matching t
      ido-use-filename-at-point 'guess
      id-create-new-buffer 'always
      ido-save-directory-list-file (expand-file-name "ido.hist" savefile-dir)
      ido-default-file-method 'selected-window
      ido-auto-merge-work-directories-length -1)

(ido-mode +1)
(ido-ubiquitous-mode +1)
(ido-vertical-mode +1)
(flx-ido-mode +1)

(require 'smex)
(setq smex-save-file (expand-file-name ".smex-items" savefile-dir))
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(provide 'my-ido)

;;; my-ido.el ends here
