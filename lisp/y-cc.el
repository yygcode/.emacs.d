;;; y-cc.el --- C/C++ mode config -*- lexical-binding:t -*-

;; Copyright (C) 2017-2020 yonggang.yyg<yygcode@gmail.com>

;; Author: yonggang.yyg<yygcode@gmail.com>
;; Maintainer: yonggang.yyg<yygcode@gmail.com>
;; Keyword: Emacs Company Configuration
;; Homepage: https://ycode.org
;; URL: http://github.com/yygcode/.emacs.d

;; SPDX-License-Identifier: GPL-2.0-or-later

;;; Commentary:

;; Cross reference config for different major-mode.

;;; Code:

(require 'y-auxiliary)
(require 'y-package)

(require 'cc-mode)

(define-key c-mode-map (kbd "C-c C-c") #'y/comment-or-uncomment)
(define-key c++-mode-map (kbd "C-c C-c") #'y/comment-or-uncomment)

(use-package modern-cpp-font-lock
  :diminish modern-c++-font-lock-mode
  :hook
  (c++-mode . modern-c++-font-lock-mode))

(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hh\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))

(provide 'y-cc)

;;; y-cc.el ends here
