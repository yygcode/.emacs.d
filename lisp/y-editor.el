;;; y-editor.el --- Editor behavior config -*- lexical-binding:t -*-

;; Copyright (C) 2017-2020 yonggang.yyg<yygcode@gmail.com>

;; Author: yonggang.yyg<yygcode@gmail.com>
;; Maintainer: yonggang.yyg<yygcode@gmail.com>
;; Keyword: Emacs Behavior Autosave Backup Bookmark
;; Homepage: https://ycode.org
;; URL: http://github.com/yygcode/.emacs.d

;; SPDX-License-Identifier: GPL-2.0-or-later

;;; Commentary:

;; Miscellaneous editor behavior config.

;;; Code:

(require 'y-auxiliary)
(require 'y-package)

(require 'autorevert)
(require 'bookmark)
(require 'desktop)
(require 'files)
(require 'flyspell)
(require 'hl-line)
(require 'recentf)
(require 'savehist)
(require 'saveplace)
(require 'simple)
(require 'url-cache)
(require 'url-cookie)
(require 'url-history)

(setq-default indent-tabs-mode nil
              tab-width 8)

(let ((backup-dir (concat y/autofiles-directory "backup/")))
  (y/make-directory backup-dir)
  (setq backup-directory-alist `((".*" . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,backup-dir t))
        auto-save-interval 300
        auto-save-list-file-prefix
        (concat y/autofiles-directory
                (file-name-base auto-save-list-file-prefix))))

(setq line-move-visual nil
      create-lockfiles nil
      read-quoted-char-radix 16
      find-file-suppress-same-file-warnings t
      make-backup-files nil
      case-fold-search t
      split-width-threshold 180
      split-height-threshold 100
      scroll-margin 0
      scroll-conservatively 1000
      scroll-preserve-screen-position t
      kill-ring-max 500
      kill-whole-line t
      confirm-kill-processes nil
      large-file-warning-threshold (* 256 1024 1024) ;; 256MB
      default-directory "~/"
      blink-matching-paren nil
      bookmark-default-file (concat y/autofiles-directory "bookmark.el")
      save-interprogram-paste-before-kill t
      set-mark-command-repeat-pop t
      truncate-lines nil
      truncate-partial-width-windows nil
      url-cache-directory (concat y/autofiles-directory "url/cache")
      url-cookie-file (concat y/autofiles-directory "url/cookie")
      url-history-file (concat y/autofiles-directory "url/history")
      )

(fset 'yes-or-no-p 'y-or-n-p)

(setq savehist-file (concat y/autofiles-directory "history"))
(savehist-mode +1)

(setq auto-revert-verbose nil) ;; disable reverting buffer message
(global-auto-revert-mode +1)

(global-hl-line-mode +1)
(delete-selection-mode +1)
(transient-mark-mode +1)

(setq save-place-file (concat y/autofiles-directory "places"))
(save-place-mode +1)

(setq recentf-save-file (concat y/autofiles-directory "recentf"))
(recentf-mode +1)

(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)
(diminish 'flyspell-mode)

(dolist (x '(narrow-to-roegion narrow-to-page narrow-to-defun
             upcase-region downcase-region erase-buffer))
  (put x 'disabled nil))

;; Restore very slow in Windows, disable it
(unless (or (string= system-type "winows-nt")
            (getenv "EMACS_Y_INTERNAL_ESUP_PROFILER"))
  (setq desktop-path `(,y/autofiles-directory)
        desktop-dirname y/autofiles-directory
        desktop-load-locked-desktop nil ;; Do not load if locked
        desktop-save t ;; no ask
        ;; restoring frame is generally unexpected
        desktop-restore-frames nil)
  (desktop-save-mode +1))

(setq-default
 inhibit-splash-screen t
 initial-scratch-message
 (concat
  ";; This buffer is for text that is not saved, and for Lisp evaluation.\n"
  ";; To create a file, visit it with C-x C-f and enter text in its buffer.\n\n"
  ";; Happy hacking " (or user-login-name "<yanyg>") " - Emacs loves you!\n\n"))

(provide 'y-editor)

;;; y-editor.el ends here
