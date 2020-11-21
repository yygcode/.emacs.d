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

(defgroup y/publish-html nil
  "Publish html."
  :prefix "y/publish-html-"
  :group 'y/publish-html)

(defun y/org-publish-find-date(entry plist)
  "Fix bug of org-publish-find-date for ENTRY, PLIST."
  (let ((date (org-publish-find-property entry :date plist)))
    ;; format-time-string don't know YYYY-MM-DD;
    ;; so convert YYYY-MM-DD to YYYY-MM-DD 00:00
    (setq date (if date (org-no-properties
                         (org-element-interpret-data date))
                 (current-time-string)))
    ;; the regexp is not correct, but it's enough here.
    (unless (string-match "[0-9][0-9]:[0-9][0-9]" date)
      (setq date (concat date " 00:00")))
    (safe-date-to-time date)))
(advice-add 'org-publish-find-date :override #'y/org-publish-find-date)

(defun y/org-publish-find-date-string(entry plist)
  "Fix bug of org-publish-find-date fpr ENTRY, PLIST."
  (format-time-string "%F" (y/org-publish-find-date entry plist)))

(defun y/blog-sitemap(title &optional list)
  "Generate sitemap contents with TITLE and LIST."
  (let ((contents
         (with-temp-buffer
           (insert (org-list-to-org list))
           (goto-char (point-min))
           (when (re-search-forward "[[file:theindex.org][Index]]" nil t)
             (move-beginning-of-line nil)
             (kill-line))
           (buffer-string))))
    (concat "#+TITLE: " title "\n" contents)))

(defun y/blog-sitemap-format-entry(entry style project)
  "Format sitemap entry for ENTRY STYLE PROJECT."
  (cond ((not (directory-name-p entry))
         (format "[%s] [[file:%s][%s]]"
                 (y/org-publish-find-date-string entry project)
                 entry
                 (org-publish-find-title entry project)))
        ((eq style 'tree)
         ;; Return only last subdir.
         (file-name-nondirectory (directory-file-name entry)))
        (t entry)))

(defun y/code-snippets-sitemap(title &optional list)
  "Generate sitemap contents with TITLE and LIST."
  (let ((contents
         (with-temp-buffer
           (insert (org-list-to-org list))
           (goto-char (point-min))
           (when (re-search-forward "[[file:theindex.org][Index]]" nil t)
             (move-beginning-of-line nil)
             (kill-line))
           (buffer-string))))
    (concat "#+TITLE: " title "\n" contents)))

(defun y/code-snippets-sitemap-format-entry(entry style project)
  "Format sitemap entry for ENTRY STYLE PROJECT."
  (cond ((not (directory-name-p entry))
         (format "[%s] [[file:%s][%s]]"
                 (y/org-publish-find-date-string entry project)
                 entry
                 (org-publish-find-title entry project)))
        ((eq style 'tree)
         ;; Return only last subdir.
         (file-name-nondirectory (directory-file-name entry)))
        (t entry)))

(defun y/sitemap-format-entry(entry style project)
  "Add Date Prefix for ENTRY STYLE PROJECT."
  (cond ((not (directory-name-p entry))
         (format "[%s] [[file:%s][%s]]"
                 (y/org-publish-find-date-string entry project)
                 entry
                 (org-publish-find-title entry project)))
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
    (concat "#+TITLE: " title "------\n\n" contents)))

(defcustom y/html-snippets-dir
  "~/docs/website/html"
  "Html snippets directory for org publish."
  :type 'string
  :group 'y/publish-html)

(defun y/org-html-template(args)
  "Enhance html contents.
ARGS is the argument list."
  (let* ((contents (nth 0 args))
         (plist (nth 1 args))
         (utterances-theme (or (plist-get plist :y/utterances-theme)
                               "utterances-light.html"))
         (utterances-theme-file
          (concat y/html-snippets-dir "/" utterances-theme))
         (sitemap-filename (plist-get plist :sitemap-filename))
         (html-head (plist-get plist :y/html-head-file))
         (html-head-sitemap (plist-get plist :y/html-head-sitemap-file))
         (html-head-theindex (plist-get plist :y/html-head-theindex-file))
         (html-preamble (plist-get plist :y/html-preamble-file))
         (html-preamble-sitemap
          (plist-get plist :y/html-preamble-sitemap-file))
         (html-preamble-theindex
          (plist-get plist :y/html-preamble-theindex-file)))

    ;; utterances
    (when (and (not (plist-get plist :y/utterances-disabled))
               (file-readable-p utterances-theme-file))
      (setf (nth 0 args)
            (concat contents "\n" (y/string-from-file utterances-theme-file))))

    (and (stringp html-head)
         (plist-put plist :html-head (y/string-from-file html-head)))
    (and (stringp html-preamble)
         (plist-put plist :html-preamble (y/string-from-file html-preamble)))

    ;; sitemap and theindex html-head
    (when (and (stringp sitemap-filename)
               (string-suffix-p sitemap-filename
                                (plist-get plist :input-file)))
      (and (stringp html-head-sitemap)
           (plist-put plist :html-head
                      (y/string-from-file html-head-sitemap)))
      (and (stringp html-preamble-sitemap)
           (plist-put plist :html-preamble
                      (y/string-from-file html-preamble-sitemap))))
    (when (string-suffix-p "theindex.org" (plist-get plist :input-file))
      (and (stringp html-head-theindex)
           (plist-put plist :html-head
                      (y/string-from-file html-head-theindex)))
      (and (stringp html-preamble-theindex)
           (plist-put plist :html-preamble
                      (y/string-from-file html-preamble-theindex))))
    args))
(advice-add 'org-html-template :filter-args #'y/org-html-template)

(defun y/project-postamble(plist)
  "Generate html postample string for PLIST."
  (y/string-from-file-safe
   (or (plist-get plist :y/postamble-file)
       ;; default postamble declares copyright.
       (concat y/html-snippets-dir "/postamble-default.html"))))

(defun y/project-preamble(plist)
  "Generate html postample string for PLIST property."
  ;; `y/org-html-template' would set proper preamble,
  ;; Reserve the function here for reference later.
  (y/string-from-file-safe (plist-get plist :y/html-preamble-file)))

(setq org-html-doctype "xhtml5"
      org-html-coding-system 'utf-8-unix
      org-html-link-home "" ;; "https://ycode.org"
      org-html-preamble #'y/project-preamble
      org-html-postamble #'y/project-postamble
      org-html-viewport nil ;; use bootstrap
      ;; disable builtin style and scripts, and config in project.
      org-html-head-include-default-style nil
      org-html-head-include-scripts nil
      org-html-htmlize-output-type 'css
      org-html-table-default-attributes nil)
;; Comments: Change `org-html-mathjax-options' and `org-html-mathjax-template'
;; if want to customize mathjax.

(let ((sites))
  (setq sites
        `(("blogs"
           :base-directory "~/docs/website/blogs"
           :publishing-directory "~/hub/github.io/blogs"
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
           :y/html-preamble-file "~/docs/website/html/preamble-blog.html"
           :y/html-head-file "~/docs/website/html/head-blog.html"
           :y/html-preamble-sitemap-file
           "~/docs/website/html/preamble-blog-sitemap.html"
           :y/html-preamble-theindex-file
           "~/docs/website/html/preamble-blog-theindex.html"
           :author "yanyg"
           :email "yygcode@gmail.com"
           :html-link-home ""
           :html-link-up ""
           :publishing-function org-html-publish-to-html
           :section-numbers t
           :htmlized-source t
           :with-toc t
           )
          ("code-snippets"
           :base-directory "~/docs/website/code-snippets"
           :publishing-directory "~/hub/github.io/code-snippets"
           :recursive t
           :headline-levels 4
           :auto-sitemap t
           :sitemap-filename "snippets.org"
           :sitemap-function y/code-snippets-sitemap
           :sitemap-format-entry y/code-snippets-sitemap-format-entry
           :sitemap-title "Code Snippets"
           :sitemap-sort-files anti-chronologically
           :sitemap-style list
           :makeindex t
           :auto-preamble t
           :y/html-preamble-file
           "~/docs/website/html/preamble-code-snippet.html"
           :y/html-head-file
           "~/docs/website/html/head-code-snippet.html"
           :y/html-preamble-sitemap-file
           "~/docs/website/html/preamble-code-snippet-sitemap.html"
           :y/html-preamble-theindex-file
           "~/docs/website/html/preamble-code-snippet-theindex.html"
           :author "yanyg"
           :email "yygcode@gmail.com"
           :html-link-home ""
           :html-link-up ""
           :publishing-function org-html-publish-to-html
           :section-numbers t
           :htmlized-source t
           :with-toc t
           )
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
           :html-head ,(y/string-from-file-safe
                        (concat y/html-snippets-dir "/head-paper.html"))
           :html-link-home "paperlist.html"
           :html-link-up "../index.html"
           :publishing-function org-html-publish-to-html
           :section-numbers t
           :htmlized-source t
           :with-toc t)
          ("perf"
           :base-directory "~/docs/website/perf"
           :base-extension "org"
           :publishing-directory "~/hub/github.io/perf"
           :recursive t
           :headline-levels 4
           :auto-sitemap t
           :sitemap-filename "sitemap.org"
           :sitemap-title "Perf SiteMap"
           :sitemap-sort-files anti-chronologically
           :sitemap-style list
           :sitemap-function y/main-sitemap
           :sitemap-format-entry y/sitemap-format-entry
           :makeindex t
           :auto-preamble t
           :author "yanyg"
           :email "yygcode@gmail.com"
           :html-head ,(y/string-from-file-safe
                        (concat y/html-snippets-dir "/head-paper.html"))
           :html-link-home "index.html"
           :html-link-up "../index.html"
           :publishing-function org-html-publish-to-html
           :section-numbers t
           :htmlized-source t
           :with-toc t)
          ("html-themes-example"
           :base-directory "~/docs/website/examples"
           :base-extension "org"
           :publishing-directory "~/hub/github.io/examples"
           :recursive t
           :headline-levels 4
           :auto-sitemap t
           :sitemap-filename "examples.org"
           :sitemap-title "ExampleList"
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
           :sitemap-filename "sitemap.org"
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
