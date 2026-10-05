;;; early-init.el --- Make init faster.  -*- lexical-binding: t; -*-
;; Copyright (C) 2010-2021 Francisco Soto
;; Author: Francisco Soto <ebobby@ebobby.org>
;; URL: https://github.com/ebobby/emacs.d
;;
;; This file is not part of GNU Emacs.
;; This file is free software.
;;; Commentary:
;;; Code:

;; Emacs 27.1 introduced early-init.el, which is run before init.el, before
;; package and UI initialization happens, and before site files are loaded.

;; Maximize garbage collection threshold to reduce initialization time.
(setq gc-cons-threshold most-positive-fixnum)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code.
(setq load-prefer-newer t)

;; The libgccjit bundled with Emacs.app derives the deployment target from the
;; Darwin kernel version (27 -> "18.0"), which clang rejects on macOS 26+.
;; Pass the real macOS version so native compilation works.
(when (and (eq system-type 'darwin)
           (native-comp-available-p))
  (setq native-comp-driver-options
        (list "-Wl,-w"
              (concat "-mmacosx-version-min="
                      (car (process-lines "sw_vers" "-productVersion"))))))

;; Set up the initial frame before it is drawn, avoiding a flash of the
;; default frame being resized and stripped of its bars.
(setq frame-inhibit-implied-resize t)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
;; Only the initial frame: child frames (popups) inherit `default-frame-alist',
;; and a maximized child frame can't be resized to fit its contents.
(push '(fullscreen . maximized) initial-frame-alist)
(when (eq system-type 'darwin)
  (push '(font . "Monaspace Neon NF-15") default-frame-alist))

;;; early-init.el ends here
