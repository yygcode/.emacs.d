;;; y-phtml.el --- Publish html config -*- lexical-binding:t -*-

;; Copyright (C) 2017-2020 yonggang.yyg<yygcode@gmail.com>

;; Author: yonggang.yyg<yygcode@gmail.com>
;; Maintainer: yonggang.yyg<yygcode@gmail.com>
;; Keyword: Emacs Org Publish Html(Website)
;; Homepage: https://ycode.org
;; URL: http://github.com/yygcode/.emacs.d

;; SPDX-License-Identifier: GPL-2.0-or-later

;;; Commentary:

;;; Code:

(require 'y-org)

(setq org-html-doctype "xhtml5"
      org-html-coding-system 'utf-8-unix
      org-html-link-home "https://ycode.org"
      org-html-preamble nil
      org-html-postamble nil
      org-html-viewport nil ;; use bootstrap
      ;; disable builtin style and scripts, and config in project.
      org-html-head-include-default-style nil
      org-html-head-include-scripts nil
      org-html-htmlize-output-type 'css
      org-html-table-default-attributes nil)

(defun y/org-publish-find-date(entry project)
  "Fix bug of org-publish-find-date fpr ENTRY, PROJECT."
  (let* ((date (org-publish-find-property entry :date project)))
    ;; format-time-string don't know YYYY-MM-DD;
    ;; so convert YYYY-MM-DD to YYYY-MM-DD 00:00
    (if date
        (progn
          (setq date (org-no-properties
                      (org-element-interpret-data date)))
          (unless (string-match "[1-9]:[0-5]" date)
            (setq date (concat date " 00:00")))
          (setq date (safe-date-to-time date)))
      (setq date (current-time)))
    date))

(defun y/blog-sitemap(title &optional list)
  "Generate sitemap contents with TITLE and LIST."
  (let ((contents
         (with-temp-buffer
           (insert (org-list-to-org list))
           (buffer-string))))
    (concat "#+TITLE: " title "\n------\n\n" contents)))

(defun y/blog-sitemap-format-entry(entry style project)
  "Format sitemap entry for ENTRY STYLE PROJECT."
  (cond ((not (directory-name-p entry))
         (let* ((date (y/org-publish-find-date entry project)))
           (format "[%s] [[file:%s][%s]]"
                   (format-time-string "%F" date) entry
                   (org-publish-find-title entry project))))
        ((eq style 'tree)
         ;; Return only last subdir.
         (file-name-nondirectory (directory-file-name entry)))
        (t entry)))

(defun y/sitemap-format-entry(entry style project)
  "Add Date Prefix for ENTRY STYLE PROJECT."
  (cond ((not (directory-name-p entry))
         (let* ((date (y/org-publish-find-date entry project)))
           (format "[%s] [[file:%s][%s]]"
                   (format-time-string "%F" date) entry
                   (org-publish-find-title entry project))))
        ((eq style 'tree)
         ;; Return only last subdir.
         (file-name-nondirectory (directory-file-name entry)))
        (t entry)))

(defun y/main-sitemap(title &optional list)
  "Generate Sitemap Items for TITLE and LIST."
  (let ((contents
         (with-temp-buffer
           (insert (org-list-to-org list))
           (goto-char (point-min))
           (when (re-search-forward "[[file:theindex.org][Index]]" nil t)
             (move-beginning-of-line nil)
             (kill-line))
           (buffer-string))))
    (concat "#+TITLE: " title "\n------\n\n" contents)))

(let ((sites))
  (setq sites
        `(("blogs"
           :base-directory "~/docs/website/blogs"
           :base-extension "org"
           :publishing-directory "~/hub/github.io/blogs"
           :preparation-function y/publish-preparation
           :completion-function y/publish-completion
           :recursive t
           :headline-levels 4
           :auto-sitemap t
           :sitemap-filename "archives.org"
           :sitemap-function y/blog-sitemap
           :sitemap-format-entry y/blog-sitemap-format-entry
           :sitemap-title "Archives"
           :sitemap-sort-files anti-chronologically
           :sitemap-style list
           :makeindex t
           :auto-preamble t
           :author "yanyg"
           :email "yygcode@gmail.com"
           :html-link-home ""
           :html-link-up ""
           :publishing-function org-html-publish-to-html
           :section-numbers t
           :htmlized-source t
           :with-toc t)
          ("papers"
           :base-directory "~/docs/website/papers"
           :base-extension "org"
           :publishing-directory "~/hub/github.io/papers"
           :recursive t
           :headline-levels 4
           :auto-sitemap t
           :sitemap-filename "paperlist.org"
           :sitemap-title "PaperList"
           :sitemap-sort-files anti-chronologically
           :sitemap-style list
           :sitemap-function y/main-sitemap
           :sitemap-format-entry y/sitemap-format-entry
           :makeindex t
           :auto-preamble t
           :author "yanyg"
           :email "yygcode@gmail.com"
           :html-link-home ""
           :html-link-up ""
           :publishing-function org-html-publish-to-html
           :section-numbers t
           :htmlized-source t
           :with-toc t)
          ("main"
           :base-directory "~/docs/website"
           :base-extension "org"
           :publishing-directory "~/hub/github.io"
           :recursive nil
           :headline-levels 4
           :auto-sitemap t
           :sitemap-function y/main-sitemap
           :sitemap-format-entry y/sitemap-format-entry
           :sitemap-filename "main.org"
           :sitemap-title "Main Archives"
           :sitemap-sort-files anti-chronologically
           :sitemap-style list
           :makeindex t
           :auto-preamble t
           :publishing-function org-html-publish-to-html
           :section-numbers t
           :htmlized-source t
           :with-toc t)
          ("attachments"
           :base-directory "~/docs/website"
           :base-extension ,(concat "\\|css\\|js\\|"
                                    "png\\|jpg\\|gif\\|pdf\\|"
                                    "mp3\\|ogg\\|mp4\\|ttf")
           :publishing-directory "~/hub/github.io"
           :recursive t
           :publishing-function org-publish-attachment)
          ("website"
           :components ("main" "blogs" "papers" "attachments"))))
  (y/append-to-list 'org-publish-project-alist sites))

(provide 'y-phtml)

;;; y-phtml.el ends here
