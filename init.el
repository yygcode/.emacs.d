;;; ~/.emacs.d/init.el --- Emacs Initialization Entry

;; Copyright (C) 2017-2020 yonggang.yyg<yygcode@gmail.com>

;; Author: yonggang.yyg<yygcode@gmail.com>
;; Maintainer: yonggang.yyg<yygcode@gmail.com>
;; Keyword: Emacs Initialization Entry
;; Homepage: https://ycode.org; http://ycode.org;
;; URL: http://github.com/yygcode/.emacs.d

;; SPDX-License-Identifier: GPL-2.0-or-later

;;; Commentary:

;;; Code:

(defconst y/startup-begin-seconds (float-time))

(when (version< emacs-version "26.3")
  (warn "The config was not tested before emacs 26.3"))

(setq load-prefer-newer t
      gc-cons-threshold (* 128 1024 1024))

(prefer-coding-system 'utf-8-unix)
(when (string-equal current-language-environment "Chinese-GBK")
  (setq default-process-coding-system '(utf-8 . chinese-gbk)))

(defconst y/lisp-directory
  (expand-file-name "lisp" user-emacs-directory)
  "Custom LISP file directory.")
(add-to-list 'load-path y/lisp-directory)

(require 'y-auxiliary)
(require 'y-package)

(defconst y/user-init-config
  (expand-file-name "config.org" user-emacs-directory)
  "File name, including directory, of initialization file by org-babel.")

(y/make-directory y/autofiles-directory)

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

;; prevent emacs mess up init.el.
(setq custom-file (concat y/autofiles-directory "custom-auto.el"))
;; NOTE: All config is maintained manally. If you want some temporary
;; configuration to take effect permanently, open the below comment:
;; (load custom-file t)

(byte-recompile-directory y/lisp-directory 0)

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
  (setq org-support-shift-select t
        org-src-fontify-natively t))
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
