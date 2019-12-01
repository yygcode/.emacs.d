;;; y-package.el --- Package management  -*- lexical-binding:t -*-

;; Copyright (C) 2017-2020 yonggang.yyg<yygcode@gmail.com>

;; Author: yonggang.yyg<yygcode@gmail.com>
;; Maintainer: yonggang.yyg<yygcode@gmail.com>
;; Keyword: Emacs Behavior Autosave Backup Bookmark
;; Homepage: https://ycode.org
;; URL: http://github.com/yygcode/.emacs.d

;; SPDX-License-Identifier: GPL-2.0-or-later

;;; Commentary:

;; Package management.

;; Enviroment Variable Description
;;
;; EMACS_NO_MIRROR:
;;   Set env to 1 to disable package-archives mirror, and install packages
;;   from official sites.  The config uses tsinghua mirror by default because
;;   the official melpa sites are blocked by GFW.
;;   e.g.: export EMACS_NO_MIRROR=1
;;
;; EMACS_MIRROR_HTTP
;;   Set env to 1 install packages from the mirror site by http protocol.
;;   Do not set this env unless your system does not support SSL/TLS(https).
;;   e.g.: export EMACS_MIRROR_HTTP=1
;;
;; If both env EMACS_NO_MIRROR and EMACS_MIRROR_HTTP are set, EMACS_NO_MIRROR
;; will override EMACS_MIRROR_HTTP.

;; http_proxy, https_proxy
;;   Proxy environment.
;;   e.g.: export https_proxy=http://127.0.0.1:8888

;;; Code:

(require 'package)

(defconst y/package-archives-melpa
  '(("melpa-stable" . "https://stable.melpa.org/packages/")
    ("melpa" . "https://melpa.org/packages/")
    ("gnu" . "https://elpa.gnu.org/packages/")
    ("org" . "http://orgmode.org/elpa/")
    ("marmalade" . "https://marmalade-repo.org/packages/"))
  "MELPA package source from official site.")
(defconst y/package-archives-mirror
  '(("melpa-stable" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")
    ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
    ("gnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
    ("org" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/"))
  "Mirror package source from tsinghua.")
(defconst y/package-archives-mirror-http
  '(("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")
    ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
    ("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
    ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/"))
  "Mirror package source using http protocol from tsinghua.")

(setq package-archives
      (cond ((equal (getenv "EMACS_NO_MIRROR") "1") y/package-archives-melpa)
            ((equal (getenv "EMACS_MIRROR_HTTP") "1")
             y/package-archives-mirror-http)
            (t y/package-archives-mirror)))

(package-initialize)

;; Downloads archive contents if not exists for startup performance. But the
;; old archive contents may cause the package install to fail, then try execute
;; function package-refresh-contents manually.
(unless package-archive-contents (package-refresh-contents))

;; use-package simplifies emacs packages install and config.
;; GitHub: https://github.com/jwiegley/use-package
;; HomePage: https://jwiegley.github.io/use-package/
(unless (or (package-installed-p 'use-package)
            (package-install 'use-package))
  (error "Install use-package failed"))
(eval-and-compile
  (require 'use-package)
  (setq use-package-always-ensure t
        use-package-always-pin "melpa-stable"
        use-package-always-defer t))

;; quelpa - Install Emacs Lisp packages from source code.
;; https://github.com/quelpa/quelpa
;; disable auto-upgrade for startup performance
;; call y/upgrade-quelpa manually if necessary
(use-package quelpa
  :pin melpa
  :init
  (setq quelpa-checkout-melpa-p t
        quelpa-self-upgrade-p nil
        quelpa-update-melpa-p nil
        quelpa-stable-p t))

;; Provide quelpa option to use-package
;; https://github.com/quelpa/quelpa-use-package
(use-package quelpa-use-package
  :pin melpa
  :init
  (require 'quelpa-use-package)
  ;; I set use-package-always-ensure, so need advice here
  ;; https://github.com/quelpa/quelpa-use-package#overriding-use-package-always-ensure
  (quelpa-use-package-activate-advice))

(defun y/package-quelpa-upgrade()
  "Upgrade quelpa package."
  (interactive)
  (unless (package-installed-p 'quelpa) (package-install 'quelpa))
  (require 'quelpa)
  (quelpa-self-upgrade)
  (quelpa-upgrade)
  (quelpa-checkout-melpa)
  (with-temp-buffer
    (url-insert-file-contents
     "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))

(defun y/package-install(pkg &optional dont-select)
  "Execute `package-install' PKG DONT-SELECT to install package.
Auto execute `package-refresh-contents' then `package-install' if failed."
  (if (package-installed-p pkg)
      t
    (condition-case err
        (package-install pkg dont-select)
      (error
       (message "Install `%s' failed(err:%s), try refresh archive contents"
                pkg err)
       (package-refresh-contents)
       (package-install pkg dont-select)))))

(defun y/packages-install(&rest pkgs)
  "Install PKGS one by one.  See `y/package-install' for more details."
  (let ((ret t))
    (dolist (pkg pkgs)
      (unless (y/package-install pkg)
        (warn "Install package `%s' failed" pkg)
        (setq ret nil)))
    ret))

(defun y/packages-require(&rest pkgs)
  "Require PKGS one by one.  See `require' for more details."
  (let ((ret t))
    (dolist (pkg pkgs)
      (unless (and (package-installed-p pkg) (require pkg nil t))
        (warn "Require `%s' failed." pkg)
        (setq ret nil)))
    ret))

(defmacro y/packages-install-and-require(&rest pkgs)
  "Install and require PKGS one by one."
  `(unless (and (y/packages-install ,pkgs)
               (y/packages-require ,pkgs))
    (warn "Packages(%s) install or require failed." ,pkgs)
    nil)
  t)

(use-package diminish)

(provide 'y-package)

;;; y-package.el ends here
