;;; y-org.el --- Orgmode config -*- lexical-binding:t -*-

;; Copyright (C) 2017-2020 yonggang.yyg<yygcode@gmail.com>

;; Author: yonggang.yyg<yygcode@gmail.com>
;; Maintainer: yonggang.yyg<yygcode@gmail.com>
;; Keyword: Emacs Org Mode
;; Homepage: https://ycode.org
;; URL: http://github.com/yygcode/.emacs.d

;; SPDX-License-Identifier: GPL-2.0-or-later

;;; Commentary:

;;; Code:

(require 'y-auxiliary)
(require 'y-keymap)
(require 'y-package)

(y/packages-install 'htmlize 'org 'org-plus-contrib 'org-bullets)
(require 'htmlize)
(require 'org)
(require 'org-bullets)
;; (require 'org-src)
(require 'ox)
(require 'ox-html)
(require 'ox-publish)
(require 'ox-rss)

(setq-default org-display-custom-times t)
(setq org-src-fontify-natively t
      org-adapt-indentation nil
      org-link-file-path-type 'relative
      org-support-shift-select t
      org-directory "~/docs"
      ;; relative to `org-directory'
      org-agenda-files (list "/gtd" "/private" "/website")
      org-default-notes-file (concat org-directory "/notes.org")
      org-time-stamp-custom-formats '("%F" .
                                      "%F %H:%M"))
(add-to-list 'org-structure-template-alist '("p" . "preview"))

(y/keymap-set-key (kbd "C-c c") #'org-capture)
(y/keymap-set-key (kbd "C-c a") #'org-agenda)
(y/keymap-set-key (kbd "C-c t") #'org-insert-structure-template)

(add-hook 'org-mode-hook #'org-bullets-mode)
;; (add-hook org-src-mode-hook #'(lambda() (flycheck-mode -1)))

;; export/publish config
(defvar y/autofiles-directory)
(setq org-export-default-language "zh-CN"
      org-export-time-stamp-file nil
      org-export-with-priority t
      org-export-with-toc 2
      ;; use sub scripts like this: a_{b}, a^{b}
      org-export-with-sub-superscripts '{}
      org-publish-timestamp-directory
      (concat y/autofiles-directory "org-publish-timestamps/"))

(provide 'y-org)

;;; y-org.el ends here
