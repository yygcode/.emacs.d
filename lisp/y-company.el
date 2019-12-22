;;; ~/.emacs.d/lisp/y-init-company.el --- Company Config

;; Copyright (C) 2017-2019 yonggang.yyg<yygcode@gmail.com>

;; Author: yonggang.yyg<yygcode@gmail.com>
;; Maintainer: yonggang.yyg<yygcode@gmail.com>
;; Keyword: Emacs Company Configuration
;; Homepage: https://ycode.org; http://ycode.org;
;; URL: http://github.com/yygcode/.emacs.d

;; SPDX-License-Identifier: GPL-2.0-or-later

;;; Commentary:

;; Company config for different major-mode.

;;; Code:

;; https://company-mode.github.io/
;; https://github.com/company-mode/company-mode

(require 'cc-mode)

(require 'y-auxiliary)
(require 'y-package)

(y/package-install 'company)

(require 'company)
(require 'company-clang)

(use-package company
  :diminish
  :init
  (setq company-show-numbers t
        company-idle-delay 0.1
        company-minimum-prefix-length 4
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above nil
        company-tooltip-limit 10
        company-selection-changed t
        company-selection-wrap-around t
        company-require-match nil)
  :config
  (bind-key [remap completion-at-point] #'company-complete)
  (bind-key [remap complete-symbol] #'company-complete)
  (bind-key [remap complete-tag] #'company-complete)
  (bind-key [remap completion-at-point-functions] #'company-complete)
  (require 'company-clang)
  (setq company-clang-modes nil)
  :hook
  (after-init . global-company-mode)
  :custom-face
  (company-tooltip ((t (:foreground "orange1"))))
  (company-tooltip-selection ((t (:foreground "orange1"
                                  :background "DarkOliveGreen4"))))
  :bind
  (:map c-mode-base-map
        ("C-x <tab>"   . company-complete)
        ("C-x C-x c s"    . company-semantic)
        ("C-x C-x c g"    . company-gtags))
  (:map company-active-map
          ("<tab>"   . company-select-next)
          ("S-<tab>" . company-select-previous)
          ("C-n"     . company-select-next)
          ("C-p"     . company-select-previous)
          ("C-k"     . company-complete-selection)
          ;; company-quickhelp has no map, used map here.
          ("C-h" . company-quickhelp-manual-begin)
          ))

;; https://github.com/expez/company-quickhelp
(use-package company-quickhelp
  :init
  (setq company-quickhelp-delay nil
        company-quickhelp-max-lines 30)
  :hook
  (company-mode . company-quickhelp-mode))

;; If you like replace helm-company frontend.
;; (use-package helm-company
;;   :bind
;;   ("C-c h c" . helm-company))

(use-package company-c-headers
  :pin melpa
  :bind
  ("C-c , f" . company-c-headers))

(defvar y/company-backends-c-common
  '(company-c-headers
    ;; company-semantic ;; very slow, disable it.
    company-gtags
    company-keywords
    company-yasnippet)
  "Company-backends depends on compiler.")

(defun y/company-c-mode-common()
  "Config company hook for C/C++."
  (setq company-backends y/company-backends-c-common
        company-idle-delay .2))

(defun y/company-elisp-hook()
  "Config company hook for elisp."
  (local-set-key "\t" #'company-indent-or-complete-common)
  (setq company-backends '((company-elisp
                            company-yasnippet
                            company-files
                            company-ispell))
        company-idle-delay 0))

(defun y/company-text-hook()
  "Config company hook for text."
  (setq company-backends '((company-abbrev
                            company-dabbrev
                            company-files
                            company-bbdb
                            company-yasnippet
                            company-ispell))
        company-idle-delay 0.2))

(defun y/company-hook()
  "Comelete anything hook."
  (make-local-variable 'company-backends)
  (make-local-variable 'company-idle-delay)
  (and company-mode ;; do nothing if nil
       (cond ((or (equal major-mode 'emacs-lisp-mode)
                  (equal major-mode 'lisp-interaction-mode))
              (y/company-elisp-hook))
             ((or (equal major-mode 'c-mode)
                  (equal major-mode 'c++-mode))
              (y/company-c-mode-common))
             ;; default to text mode, so add special above here.
             (t
              (y/company-text-hook)))))
(add-hook 'company-mode-hook #'y/company-hook)

;; Backup things

;; (use-package irony
;;   :diminish
;;   :config
;;   (progn
;;     (unless (irony--find-server-executable)
;;       (call-interactively #'irony-install-server)))
;;   :hook
;;   (c-mode-common . irony-mode)
;;   (irony-mode . irony-cdb-autosetup-compile-options))

;; Disable semantic because it's very slowly
;; (defun y/semantic-mode()
;;   "Semantic mode config if clang not exists."
;;   (semantic-mode 1)
;;   (semantic-default-c-setup))

;; ;; https://www.gnu.org/software/emacs/manual/html_mono/semantic.html
;; (use-package semantic
;;   :config
;;   (global-semantic-idle-scheduler-mode 1)
;;   (global-semanticdb-minor-mode 1)
;;   (global-semantic-idle-summary-mode 1)
;;   (global-semantic-mru-bookmark-mode 1)
;;   (global-semantic-stickyfunc-mode 1)
;;   ;; (global-semantic-idle-local-symbol-highlight-mode 1)
;;   ;; (custom-set-faces
;;   ;;  '(semantic-idle-symbol-highlight
;;   ;;    ((t (:foreground "darkgoldenrod" :background "gray20" :weight bold)))))
;;   ;; (semanticdb-enable-gnu-global-databases 'c-mode t)
;;   ;; (semanticdb-enable-gnu-global-databases 'c++-mode t)
;;   ;; Turn off follow modes default
;;   ;; (global-semantic-highlight-func-mode 1)
;;   ;; (global-semantic-highlight-edits-mode 1)
;;   ;; (global-semantic-idle-completions-mode 1)
;;   ;; (global-semantic-show-unmatched-syntax-mode 1)
;;   ;; (global-semantic-decoration-mode 1)
;;   (setq semantic-idle-scheduler-idle-time 1
;;         semantic-idle-scheduler-work-idle-time 1
;;         semantic-displayor-tooltip-initial-max-tags 10
;;         semantic-displayor-tooltip-mode "verbose"
;;         ;; semanticdb-implied-include-tags t
;;         )
;;   (advice-add 'semantic-ia-fast-jump :before
;;               #'(lambda(point)
;;                   "Push marker for jump back."
;;                   (xref-push-marker-stack)))
;;   :bind
;;   ("C-c , m" . semantic-mode) ;; global-map
;;   ;; see y-tags.el
;;   ;; ("M-."     . semantic-ia-fast-jump)
;;   (:map semantic-mode-map
;;         ("C-c , ." . semantic-ia-fast-jump)
;;         ("C-c , r" . semantic-symref)
;;         ("C-c , c" . semantic-ia-complete-symbol)
;;         ("M-."     . semantic-ia-fast-jump))
;;   :hook
;;   (c-mode-common . y/semantic-mode))

;; (use-package rtags
;;   :init
;;   (setq rtags-display-result-backend 'helm)
;;   :bind
;;   (:map c-mode-base-map
;;    ("C-c r ." . rtags-find-symbol-at-point)
;;    ("C-c r r" . rtags-find-references-at-point)
;;    ("C-c r s" . rtags-display-summary)))
;; (use-package helm-rtags)
;; (use-package company-rtags)

(provide 'y-company)

;;; y-company.el ends here
