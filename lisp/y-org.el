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
(require 'org-capture)
;; (require 'org-src)
(require 'ox)
(require 'ox-html)
(require 'ox-publish)
(require 'ox-rss)

(defconst y/org-directory (expand-file-name "~/docs")
  "Directory for org mode.")
(defvar y/gtd-directory (concat y/org-directory "/gtd")
  "Getting things done directory.")
(defun y/gtd-file-meeting()
  "GTD filename for meeting."
  (concat y/gtd-directory "/meeting.org"))
(defun y/gtd-file-code()
  "GTD filename for code."
  (concat y/gtd-directory "/code.org"))

(defun y/gtd-file-idea()
  "GTD filename for idea."
  (concat y/gtd-directory "/idea.org"))

(defun y/gtd-file-blog()
  "GTD filename for blog."
  (concat y/gtd-directory "/blog.org"))

(defun y/gtd-file-task()
  "GTD filename for task."
  (concat y/gtd-directory "/task.org"))

(defvar y/gtd-file-archive
  "GTD filename for refile."
  (concat y/gtd-directory "/archive.org"))

(setq-default org-display-custom-times t)
(setq org-src-fontify-natively t
      org-adapt-indentation nil
      org-link-file-path-type 'relative
      org-support-shift-select t
      org-directory y/org-directory
      org-default-notes-file (concat y/org-directory "/notes.org")
      org-agenda-files (list y/gtd-directory
                             org-default-notes-file)
      org-refile-targets '((org-agenda-files :maxlevel . 3))
      ;; org-refile-targets '((y/gtd-file-archive :maxlevel . 3))
      org-time-stamp-custom-formats '("<%F %a>" .
                                      "<%F %a %H:%M>"))
(add-to-list 'org-structure-template-alist '("p" . "preview"))

(setq org-capture-templates
      '(("m" "Meeting" entry
         (file+headline y/gtd-file-meeting "Meetings")
         "* %?\nSCHEDULED: %^T\nFrom: %a\n:END:\n")
        ("c" "Code" entry
         (file+headline y/gtd-file-code "CodeRef")
         "* %?\n%a\n:END:\n")
        ("i" "Idea" entry
         (file+headline y/gtd-file-idea "Ideas")
         "* %?\nSCHEDULED: %T\n:END:\n")
        ("b" "Blog" entry
         (file+headline y/gtd-file-blog "Blogs")
         "* %?\nSCHEDULED: %T\n:END:\n")
        ("t" "Task" entry
         (file+headline y/gtd-file-task "Tasks")
         "* %?\nSCHEDULED: %T\n:END:\n")))

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
