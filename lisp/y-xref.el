;;; y-xref.el --- Cross reference -*- lexical-binding:t -*-

;; Copyright (C) 2017-2020 yonggang.yyg<yygcode@gmail.com>

;; Author: yonggang.yyg<yygcode@gmail.com>
;; Maintainer: yonggang.yyg<yygcode@gmail.com>
;; Keyword: Emacs Company Configuration
;; Homepage: https://ycode.org
;; URL: https://github.com/yygcode/.emacs.d

;; SPDX-License-Identifier: GPL-2.0-or-later

;;; Commentary:

;; Cross reference config for different major-mode.

;;; Code:

(require 'xref)

(require 'y-auxiliary)
(require 'y-browse)
(require 'y-keymap)

(y/env-set "GTAGSLIBPATH" "/usr")

(add-hook 'y/lisp-modes-hook
          #'(lambda()
              "xref keybind for lisp-mode."
              (define-key global-map
                (kbd "M-.") #'xref-find-definitions)
              (define-key global-map
                (kbd "M-,") #'xref-pop-marker-stack)
              (define-key global-map
                (kbd "M-r") #'xref-find-references)))

(use-package counsel-gtags
  :diminish
  :init
  (setq counsel-gtags-auto-update t
        counsel-gtags-ignore-case t
        counsel-gtags-path-style 'root
        counsel-gtags-update-interval-second nil)
  ;; (advice-add 'counsel-gtags--command-options
  ;;             :filter-args
  ;;             #'(lambda(args)
  ;;                 "Add nearness to option."
  ;;                 (push "--nearness" (nth 1 args))
  ;;                 args))
  :bind
  (:map counsel-gtags-mode-map
        ("M-."     . counsel-gtags-dwim)
        ("M-,"     . counsel-gtags-go-backward)
        ("C-c g s" . counsel-gtags-find-symbol)
        ("C-c g ." . counsel-gtags-dwim)
        ("C-c g ," . counsel-gtags-go-backward)
        ("C-c g d" . counsel-gtags-find-definition)
        ("C-c g r" . counsel-gtags-find-reference)
        ("C-c g f" . counsel-gtags-find-file)
        ("C-c g C" . counsel-gtags-create-tags)
        ("C-c g u" . counsel-gtags-update-tags))
  ;; TODO(yonggang.yyg): show error code 1 if input symbol
  ;; (:map y/browse-mode-map
  ;;       ("." . counsel-gtags-dwim)
  ;;       ("," . counsel-gtags-go-backward))
  ;; :hook
  ;; (c-mode-common . counsel-gtags-mode)
  )

(use-package ggtags
  :diminish
  :init
  (setq ggtags-sort-by-nearness t
        ggtags-update-on-save t
        ggtags-global-ignore-case t
        ggtags-global-search-libpath-for-reference t)
  :bind
  (:map ggtags-mode-map
        ("M-." . ggtags-find-tag-dwim)
        ("C-c g ." . ggtags-find-tag-dwim)
        ("C-c g s" . ggtags-find-other-symbol)
        ("C-c g h" . ggtags-view-tag-history)
        ("C-c g r" . ggtags-find-reference)
        ("C-c g f" . ggtags-find-file)
        ("C-c g C" . ggtags-create-tags)
        ("C-c g c" . ggtags-completion-at-point)
        ("C-c g u" . ggtags-update-tags))
  ;; :hook
  ;; disable ggtags, I use counsel-gtags
  ;; (c-mode-common . ggtags-mode)
  )

(use-package helm-gtags
  :diminish
  :init
  (setq helm-gtags-auto-update t
        helm-gtags-ignore-case t
        helm-gtags-use-input-at-cursor t
        helm-gtags-pulse-at-cursor t
        helm-gtags-path-style 'absolute
        helm-gtags-update-interval-second 300)
  :bind
  (:map helm-gtags-mode-map
        ("M-."     . helm-gtags-dwim)
        ("M-," . helm-gtags-pop-stack)
        ("C-c g l" . helm-gtags-tags-in-this-function)
        ("C-c g s" . helm-gtags-find-symbol)
        ("C-c g ." . helm-gtags-dwim)
        ("C-c g d" . helm-gtags-find-tag)
        ("C-c g r" . helm-gtags-find-rtag)
        ("C-c g f" . helm-gtags-find-files)
        ("C-c g C" . helm-gtags-create-tags)
        ("C-c g u" . helm-gtags-update-tags)
        ("C-c g g" . helm-gtags-select))
  (:map y/browse-mode-map
        ("." . helm-gtags-dwim)
        ("," . helm-gtags-pop-stack))
  :hook
  (c-mode-common . helm-gtags-mode))

(defun y/xref-dwim()
  "Find definition at current point."
  (interactive)
  (if (memq major-mode y/lisp-modes)
      (xref-find-definitions (xref-backend-identifier-at-point
                              (xref-find-backend)))
    (helm-gtags-dwim)))
(defun y/xref-pop-stack()
  "Find definition at current point."
  (interactive)
  (if (memq major-mode y/lisp-modes)
      (xref-pop-marker-stack)
    (helm-gtags-pop-stack)))
(y/browse-set-key (kbd ".") #'y/xref-dwim)
(y/browse-set-key (kbd ",") #'y/xref-pop-stack)

(provide 'y-xref)

;;; y-xref.el ends here
