;;; ~/.emacs.d/lisp/y-browse.el --- code BROWSE keybind

;; Copyright (C) 2017-2020 yonggang.yyg<yygcode@gmail.com>, <cppgp@qq.com>

;; Author: yonggang.yyg<yygcode@gmail.com>
;; Maintainer: yonggang.yyg<yygcode@gmail.com>
;; Keyword: Emacs Company Configuration
;; Homepage: https://ycode.org; http://ycode.org;
;; URL: http://github.com/yygcode/.emacs.d

;; SPDX-License-Identifier: GPL-2.0-or-later

;;; Commentary:

;; Company config for different major-mode.

;;; Code:

(require 'y-auxiliary)
(require 'y-keybinds)

(defgroup y/browse nil
  "Customization group for Y/Browse mode.
This mode is used to browse code."
  :package-version '(y/browse . "1.0")
  :group 'local
  :prefix "y/browse")

(defcustom y/browse-major-modes
  '(c-mode c++-mode emacs-lisp-mode lisp-interaction-mode)
  "Major-mode list if Y/Browse mode enabled."
  :group 'y/browse
  :type 'list)

(defun y/browse--ignore-key(&optional n)
  "`y/browse-mode' is used to browse code.  Ignore all input by default.
N is place holder."
  (interactive)
  (message "Ignore input key for Y/Browse mode."))

(defvar-local y/browse--set-buffer-read-only nil
  "Set to t if `buffer-read-only' state is set by `y/browse-mode'.")

(defvar-local y/browse--clear-yas-mode nil
  "Set to t if `yas-minor-mode' state is cleared by `y/browse-mode'.")

(defun y/browse--adjust-buffer(browse-mode-enabled)
  "Adjust current buffer relative modes depends on BROWSE-MODE-ENABLED."
  (if browse-mode-enabled
      (progn
        (and (setq y/browse--set-buffer-read-only (not buffer-read-only))
             (read-only-mode 1))
        (and (featurep 'yasnippet)
             (setq y/browse--clear-yas-mode yas-minor-mode)
             (yas-minor-mode -1)))
    (progn
      (when y/browse--set-buffer-read-only
        (read-only-mode -1)
        (setq y/browse--set-buffer-read-only nil))
      (when y/browse--clear-yas-mode
        (yas-minor-mode +1)
        (setq y/browse--clear-yas-mode nil)))))

(define-minor-mode y/browse-mode "Browse keybind"
  :lighter " Y/Browse"
  :init-value nil
  :keymap
  (let ((map (make-sparse-keymap)))
    ;; set all to do nothing
    (y/count-loop
     i 32 128 nil
     (define-key map (make-vector 1 i) #'y/browse--ignore-key))

    ;; http://ascii-table.com/control-chars.php
    ;; space to C-<spc>
    (define-key map (kbd "<SPC>") (kbd "C-<SPC>"))
    ;; Control character <0-1f>.
    (y/count-loop
     i 64 96 nil
     (define-key map (make-vector 1 i) (make-vector 1 (- i 64))))
    (y/count-loop
     i 97 122 nil
     (define-key map (make-vector 1 i) (make-vector 1 (- i 96))))
    ;; TODO(yonggang.yyg): Follow definition will discard interactive feature.
    ;; Press "M-." jump to one definition without candidates selective
    ;; (define-key map (kbd ".") (key-binding (kbd "M-.")))
    ;; (define-key map (kbd ",") (key-binding (kbd "M-,")))
    map)
  ;; body
  (y/browse--adjust-buffer y/browse-mode))

(defun y/browse-mode-on()
  "Active y/browse-mode."
  (interactive)
  (when (memq major-mode y/browse-major-modes)
    (y/browse-mode +1)))

(define-globalized-minor-mode y/browse-global-mode
  y/browse-mode y/browse-mode-on)

(defun y/browse-mode--global-hook()
  "Restore all buffers minor modes when `y/browse-global-mode' disabled."
  (unless y/browse-global-mode
    (let ((bl (buffer-list)))
      ;; restore old minor mode for all buffers.
      (dolist (b (buffer-list))
        (with-current-buffer b
          (when (memq major-mode y/browse-major-modes)
            (y/browse--adjust-buffer nil))
          (y/browse--adjust-buffer y/browse-global-mode))))))
(add-hook 'y/browse-global-mode-hook #'y/browse-mode--global-hook)

(defun y/browse-set-key(key command)
  "Give KEY a `y/basic-keybind-mode' binding as COMMAND.
Like as `global-set-key' but use y/browse-mode-map."
  (interactive
   (let* ((menu-prompting nil)
          (key (read-key-sequence "Set key in y/browse-mode-map: ")))
     (list key
           (read-command (format "Set key %s to command: "
                                 (key-description key))))))
  (or (vectorp key) (stringp key)
      (signal 'wrong-type-argument (list 'arrayp key)))
  (define-key y/browse-mode-map key command))

(defun y/browse-unset-key (key)
  "Remove a `y/browse-keybind-mode' binding of KEY.
KEY is a string or vector representing a sequence of keystrokes."
  (interactive "kUnset key : ")
  (y/browse-set-key key nil))

(y/basic-set-key (kbd "C-x C-x C-q") #'y/browse-global-mode)
(y/basic-set-key (kbd "C-x C-x q") #'y/browse-global-mode)

(add-hook 'after-init-hook
          #'(lambda()
              "Disable browse mode default."
              (y/browse-global-mode -1)))

(provide 'y-browse)

;;; y-browse.el ends here
