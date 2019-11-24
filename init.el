;;; ~/.emacs.d/init.el --- Emacs Initialization/Customization File

;; Copyright (C) 2017-2020 yonggang.yyg<yygcode@gmail.com>

;; Author: yonggang.yyg<yygcode@gmail.com>
;; Maintainer: yonggang.yyg<yygcode@gmail.com>
;; Keyword: Emacs Initialization Customization Configuration
;; Homepage: https://ycode.org; http://ycode.org;
;; URL: http://github.com/yygcode/.emacs.d

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING, if not see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs base customization, includes package-tool, proxy and org-babel
;; then use config.org to deep customize.
;;
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

(defconst y/startup-begin-seconds (float-time))

(when (version< emacs-version "26.3")
  (warn "The config was not tested before emacs 26.3"))

;; disable trival compile warnings
(setq byte-compile-warnings
      '(redefine callargs obsolete noruntime cl-functions interactive-only
        make-local mapcar constants suspicious lexical))

;; garbage collection change to 128MB for performance optimize
(setq gc-cons-threshold (* 128 1024 1024))

(prefer-coding-system 'utf-8-unix)
(when (string-equal current-language-environment "Chinese-GBK")
  ;; Do not use utf-8-unix . chinese-gbk-dos for
  ;; the set will cause ggtags failure
  (setq default-process-coding-system '(utf-8 . chinese-gbk)))

(defconst y/user-init-config
  (expand-file-name "config.org" user-emacs-directory)
  "File name, including directory, of initialization file by org-babel.")

(defconst y/lisp-directory
  (expand-file-name "lisp" user-emacs-directory)
  "Extended LISP file directory.")
(add-to-list 'load-path y/lisp-directory)

;; package - Simple package system for Emacs. Built-in
(require 'package)

;; compile first to ensure require load latest version.
(byte-recompile-file (concat y/lisp-directory "/y-auxiliary.el") nil 0)
(require 'y-auxiliary)

;;; Proxy environment config. e.g.: https_proxy=https://127.0.0.1:8888
(y/env-sync-partner "https_proxy" "http_proxy")
(y/add-no-proxy-sites "*.cn"
                      "*.aliyun.com"
                      "*.tmall.com"
                      "*.youku.com"
                      "*.jd.com"
                      "*.bing.com"
                      "*.baidu.com"
                      "*.csdn.net"
                      "*.qq.com")

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
(when (not package-archive-contents)
  (package-refresh-contents))

;; Set custom-file variable to prevent emacs mess up init.el.
(setq custom-file (expand-file-name ".custom-auto.el" user-emacs-directory))
;; All config is maintained manally. If you want some temporary configuration to
;; take effect permanently, open the below comment.
;; (load custom-file t)

;; use-package simplifies emacs packages install and config.
;; GitHub: https://github.com/jwiegley/use-package
;; HomePage: https://jwiegley.github.io/use-package/
(unless (or (package-installed-p 'use-package)
            (package-install 'use-package))
  (error "Install use-package failed"))
(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t)
  (setq use-package-always-pin "melpa-stable")
  (setq use-package-always-defer t))

;; quelpa - Install Emacs Lisp packages from source code.
;; https://github.com/quelpa/quelpa
(use-package quelpa
  :pin melpa
  :init
  ;; disable auto-upgrade for startup performance
  ;; call y/upgrade-quelpa manually if necessary
  (setq quelpa-checkout-melpa-p t)
  (setq quelpa-self-upgrade-p nil)
  (setq quelpa-update-melpa-p nil)
  (setq quelpa-stable-p t))

;; Provide quelpa option to use-package
;; https://github.com/quelpa/quelpa-use-package
(use-package quelpa-use-package
  :pin melpa
  :init
  (require 'quelpa-use-package)
  ;; I set use-package-always-ensure, so need advice here
  ;; https://github.com/quelpa/quelpa-use-package#overriding-use-package-always-ensure
  (quelpa-use-package-activate-advice))

(eval-and-compile
  (byte-recompile-directory y/lisp-directory 0))

;;; Compile lisp directory after package tools.
;; y-company.el complains error.  Fix later.
;; (byte-recompile-directory y/lisp-directory 0)

;;; Emacs deep config with Org-mode literate programming

;; If you want to use the latest org, use the follows config:
;; 1. Download latest package or clone repo.
;;    URL: http://orgmode.org/
;;    REPO:
;;      ~$ git clone git://orgmode.org/org-mode.git
;;      ~$ make autoloads
;; 2. add load-path
;;    (add-to-list 'load-path "~/path/to/orgdir/lisp")
;; 3. If you want contributed libraries
;;    (add-to-list 'load-path "~/path/to/orgdir/contrib/lisp" t)
;; See homepage http://orgmode.org/ for more details.

;; use-package use 'package-installed-p' to check package installed or not
;; and org is a built-in package, so use-package would ignore org package
;; but org-plus-contrib is not installed default, so I think I can force install
;; org by routine package-install but failed.
(use-package org
  :pin org
  :init
  (setq org-support-shift-select t)
  (setq org-src-fontify-natively t))
(use-package org-plus-contrib
  :pin org)

;; use env @EMACS_Y_INTERNAL_ESUP_PROFILER to prevent esub reload recursively.
(when (and (file-exists-p y/user-init-config)
           (not (string= (getenv "EMACS_Y_INTERNAL_ESUP_PROFILER") "y/esup")))
  (let* ((config-el (y/file-replace-extension y/user-init-config "el")))
    (when (file-newer-than-file-p y/user-init-config config-el)
      (require 'ob-tangle)
      (org-babel-tangle-file y/user-init-config config-el))
    (byte-recompile-file config-el nil 0 t)))

(defconst y/startup-end-seconds (float-time))
(defconst y/startup-duration-seconds (- y/startup-end-seconds
                                        y/startup-begin-seconds))
(message "==> Emacs startup takes %.2f seconds." y/startup-duration-seconds)

;;; init.el ends here
