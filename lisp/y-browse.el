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

(defun y/browse--ignore-key(&optional n)
  "`y/browse-mode' is used to browse code.  Ignore all input by default.
N is place holder."
  (interactive)
  (message "Ignore input key for y/browse mode."))

(defvar y/browse-mode-exit-hook '()
  "Hooks when exit from browse mode.")

(define-minor-mode y/browse-mode "Browse keybind"
  :lighter " Y/Browse"
  :init-value nil
  :global t
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
  (unless y/browse-mode (run-hooks 'y/browse-mode-exit-hook)))

(defun y/browse-mode-on()
  "Active y/browse-mode."
  (interactive)
  (y/browse-mode 1))

(define-globalized-minor-mode y/browse-global-mode
  y/browse-mode y/browse-mode-on)

(defvar-local y/browse--buffer-read-only nil
  "Record buffer readonly state before browse-mode enabled.")

(defun y/browse-mode-adjust()
  "Browse mode adjust."
  (if y/browse-mode
      (or (setq y/browse--buffer-read-only buffer-read-only)
          (read-only-mode 1))
    (dolist (b (buffer-list))
      (with-current-buffer b
        (unless y/browse--buffer-read-only
          (read-only-mode -1))))
    ;; (or y/browse--buffer-read-only
    ;;     (read-only-mode -1))
    ))
(add-hook 'y/browse-mode-hook #'y/browse-mode-adjust)
(add-hook 'y/browse-mode-exit-hook #'y/browse-mode-adjust)

(defvar y/browse-mode--minibuffer-disabled nil
  "The browse mode disabled by minibuffer or not.")

(defun y/browse-mode--minibuffer-maybe-deactivate()
  "Auto disable y/browse-mode in minibuffer."
  (when y/browse-mode
    (y/browse-mode -1)
    (setq y/browse-mode--minibuffer-disabled t)))

(defun y/browse-mode--minibuffer-maybe-activate()
  "Auto disable y/browse-mode in minibuffer."
  (when y/browse-mode--minibuffer-disabled
    (y/browse-mode 1)
    (setq y/browse-mode--minibuffer-disabled nil)))

(add-hook 'minibuffer-setup-hook
          #'y/browse-mode--minibuffer-maybe-deactivate)
(add-hook 'minibuffer-exit-hook
          #'y/browse-mode--minibuffer-maybe-activate)

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
