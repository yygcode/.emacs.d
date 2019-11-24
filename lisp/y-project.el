;;; ~/.emacs.d/lisp/y-project.el --- Project Config

;; Copyright (C) 2017-2020 yonggang.yyg<yygcode@gmail.com>, <cppgp@qq.com>

;; Author: yonggang.yyg<yygcode@gmail.com>
;; Maintainer: yonggang.yyg<yygcode@gmail.com>
;; Keyword: Emacs Company Configuration
;; Homepage: https://ycode.org; http://ycode.org;
;; URL: http://github.com/yygcode/.emacs.d

;; SPDX-License-Identifier: GPL-2.0-or-later

;;; Commentary:

;; Cross reference config for different major-mode.

;;; Code:

(require 'y-auxiliary)

(use-package projectile
  :diminish
  :init
  (setq projectile-sort-order 'recentf-active
        projectile-enable-caching t
        projectile-indexing-method 'alien
        projectile-completion-system 'ivy)
  (projectile-mode 1)
  ;; use counsel
  ;; :bind
  ;; ("C-c p" . projectile-command-map)
  )

(use-package counsel-projectile
  :init
  (setq counsel-projectile-org-capture-templates
        '(("t" "[${name}] Task" entry
           (file+headline "${root}/notes.org" "Tasks")
           "* TODO %?\n  %u\n  %a")
          ("m" "[${name}] Materials" entry
           (file+headline "${root}/notes.org" "Materials")
           "* %?\n  %u\n  %a")))
  (counsel-projectile-mode 1)
  :bind
  ("C-c p p" . counsel-projectile)
  ("C-c p s" . counsel-projectile-switch-project)
  ("C-c p ." . counsel-projectile-find-file-dwim)
  ("C-c p f" . counsel-projectile-find-file)
  ("C-c p d" . counsel-projectile-find-dir)
  ("C-c p b" . counsel-projectile-switch-to-buffer)
  ("C-c p a" . counsel-projectile-ag)
  ("C-c p g" . counsel-projectile-grep)
  ("C-c p c" . counsel-projectile-org-capture)
  ("C-c p A" . counsel-projectile-org-agenda))

;; support .emacs.include
;; path-system: src/include ../third-party/include
;; path-user: .

(use-package company)
(use-package company-c-headers
  :init
  (make-local-variable 'company-c-headers-path-system)
  (make-local-variable 'company-c-headers-path-user))
(use-package flycheck)

(defun y/string-from-file(filename)
  "Return FILENAME content in string."
  (let ((s nil))
    (when (and (stringp filename)
               (file-readable-p filename)
               (file-regular-p filename))
      (with-temp-buffer
        (insert-file-contents filename)
        (setq s (buffer-string))))
    s))

(defvar-local y/c-headers-path-system nil
  "List of paths to search for system.")

(defvar-local y/c-headers-path-user nil
  "List of paths to search for user.")

;; .c-headers-path format:
;; system: include
(defun y/c-header-adjust-from-string(root str)
  "Adjust c header path from string STR.
ROOT is the default directory for relative path.

string example:
system:
./src
./src/include

user:
."
  (let ((l (split-string str "\n"))
        (type-switch t)(type-path nil)(do-append))
    (dolist (v l)
      (setq do-append t)
      (pcase v
        ((or '"system:" '"user:")
         (when type-switch
           (if (equal v "system:")
               (setq type-path 'system)
             (setq type-path 'user))
           (setq type-switch nil
                 do-append nil)
           ))
        ('""
         (setq type-switch t
               do-append nil)))
      (when do-append
        (or (string-prefix-p "/" v)
            (setq v (expand-file-name v root)))
        (if (equal type-path 'system)
            (push v y/c-headers-path-system)
          (push v y/c-headers-path-user))))
    (company-mode 1)
    (setq
     company-c-headers-path-system
     (append y/c-headers-path-system
             company-c-headers-path-system)
     company-c-headers-path-user
     (append y/c-headers-path-user
             company-c-headers-path-user)
     flycheck-clang-include-path
     (append y/c-headers-path-system
             flycheck-clang-include-path)
     flycheck-clang-includes
     (append y/c-headers-path-user
             flycheck-clang-includes)
     flycheck-gcc-include-path
     (append y/c-headers-path-system
             flycheck-gcc-include-path)
     flycheck-gcc-includes
     (append y/c-headers-path-user
             flycheck-gcc-includes))))

(defun y/project-c-headers-path-adjust()
  "Adjust `y/c-headers-path-system' and `y/c-headers-path-user'."
  (when (and projectile-mode
             (projectile-project-root))
    (let* ((root (projectile-project-root))
           (s (y/string-from-file (concat root ".c-headers-path"))))
      (y/c-header-adjust-from-string root s))))
(add-hook 'c-mode-common-hook #'y/project-c-headers-path-adjust)

(provide 'y-project)

;;; y-project.el ends here
