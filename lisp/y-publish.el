;;; y-publish.el --- Publish config -*- lexical-binding:t -*-

;; Copyright (C) 2017-2020 yonggang.yyg<yygcode@gmail.com>

;; Author: yonggang.yyg<yygcode@gmail.com>
;; Maintainer: yonggang.yyg<yygcode@gmail.com>
;; Keyword: Emacs Org Publish
;; Homepage: https://ycode.org
;; URL: http://github.com/yygcode/.emacs.d

;; SPDX-License-Identifier: GPL-2.0-or-later

;;; Commentary:

;;; Code:

(require 'y-phtml)

(add-to-list 'org-publish-project-alist
             '("all" :components ("website")))

(provide 'y-publish)

;;; y-publish.el ends here
