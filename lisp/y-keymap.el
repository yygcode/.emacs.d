;;; y-keymap.el --- Custom keymaps  -*- lexical-binding:t -*-

;; Copyright (C) 2018 yanyg<yygcode@gmail.com>

;; Author: yanyg<yygcode@gmail.com>
;; Maintainer: yanyg<yygcode@gmail.com>
;; Keyword: Emacs Custom Keymap
;; URL: https://ycode.org

;; SPDX-License-Identifier: GPL-2.0-or-later

;;; Commentary:

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html
;; Key binding conventions - Do Not Rules
;; 1. Don't bind C-h following any prefix character; If you don't bind C-h,
;;    it is automatically available as a help character for listing the
;;    subcommands of the prefix character;
;; 2. Don't bind a key sequence ending in C-g, since that is commonly used to
;;    cancel a key sequence;
;; 3. Don't bind a key sequence ending in <ESC> except following another <ESC>;

;; Important Extension:
;; 1. C-x C-x: Reserved to provide more extension;
;;    `exchange-point-and-mark' binds to 'C-x C-x C-x';
;; 2. C-o: Used to `other-window'; `open-line' binds to 'C-x C-x C-o';
;; 3. Enhance keyboard: <f1>-<f9>: C-x C-x [1-9];

;;; Code:

(require 'y-auxiliary)
(require 'y-package)

(require 'cl-lib)

(y/package-install 'use-package)
(require 'use-package)

(defvar y/lisp-directory)
(defvar y/user-init-config)

(defvar y/emulation-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-o") #'other-window)
    (define-key map (kbd "C-x C-x C-o") #'open-line)
    (define-key map (kbd "C-x C-x C-x") #'exchange-point-and-mark)
    map)
  "Keymap will be added to `emulation-mode-map-alists' for each buffer.")

(cl-pushnew (list (cons 'y/emulation-map y/emulation-map))
            emulation-mode-map-alists)

(eval-and-compile
 (y/define-set-key-function
  y/emulation-set-key y/emulation-unset-key y/emulation-map))

(defgroup y/keymap nil
  "Custom keymap"
  :prefix "y/keymap-"
  :group 'y/keymap)

(defcustom y/keymap-major-modes-disabled
  '(minibuffer-inactive-mode)
  "List of major-modes to disable y/keymap."
  :type '(list minibuffer-inactive-mode)
  :version "1.0"
  :group 'y/keymap)

(defcustom y/keymap-as-use-package-map
  t
  "Use `y/keymap-mode-map' as `use-package' default map.
The original `use-package' default map is `global-map'"
  :type 'boolean
  :version "1.0"
  :group 'y/keymap)

(define-minor-mode y/keymap-mode "Basic custom keymap"
  :lighter " Y/Keymap"
  :init-value nil
  :group 'y/keymap
  :keymap
  (let ((map (make-sparse-keymap)))
    ;; keyboard enhancement
    ;; F1~F9: C-x C-x [1-9]
    (y/count-loop i 1 9 nil
                  (define-key map
                    (kbd (format "C-x C-x %d" i))
                    (kbd (format "<f%d>" i))))

    ;; search and replace
    (define-key map (kbd "C-x C-x C-s") #'isearch-forward)
    (define-key map (kbd "C-x C-x C-r") #'isearch-backward)

    ;; move
    (define-key map (kbd "C-x C-x f") #'forward-sexp)
    (define-key map (kbd "C-x C-x b") #'backward-sexp)

    ;; buffer or window
    (define-key map (kbd "C-o") #'other-window)
    (define-key map (kbd "C-x C-x o") #'open-line)
    (define-key map (kbd "C-x C-p") #'previous-buffer)
    (define-key map (kbd "C-x C-n") #'next-buffer)

    ;; edit
    ;; (define-key map (kbd "C-c C-c") #'y/comment-or-uncomment)
    (define-key map (kbd "M-d") #'y/delete-word)
    (define-key map (kbd "M-<backspace>") #'y/backward-delete-word)
    (define-key map (kbd "M-<DEL>") #'y/backward-delete-word)
    (define-key map (kbd "C-x C-x =") #'describe-char)

    ;; quick visit
    (y/define-switch-to-buffer-function-and-bind
     "*Help*" map "C-c q h")
    (y/define-switch-to-buffer-function-and-bind
     "*scratch*" map "C-c q s"
     (insert initial-scratch-message)
     (set-buffer-modified-p nil))
    (y/define-find-file-function-and-bind user-init-file map "C-c q i")
    (y/define-find-file-function-and-bind y/user-init-config map "C-c q c")
    (y/define-find-file-function-and-bind y/lisp-directory map "C-c q d")
    (y/define-find-file-function-and-bind
     (y/file-replace-extension y/user-init-config "el")
     map "C-c q C")
    (y/define-function-and-bind
     y/reload-init-file "Reload Emacs init.el"
     map "C-c q R" (load-file user-init-file))

    (y/define-kill-buffer-function-and-bind "*Help*" map "C-x C-x k h")

    ;; Emacs helper buffers.
    (y/define-function-and-bind
     y/display-startup-screen "Display startup screen."
     map "C-x C-x d s" (display-startup-screen))
    (y/define-function-and-bind
     y/display-about-screen "Display about screen."
     map "C-x C-x d a" (display-about-screen))
    (y/define-function-and-bind
     y/display-todo "Display TODO of Emacs."
     map "C-x C-x d t" (view-emacs-todo t))
    (y/define-function-and-bind
     y/display-faq "Display FAQ of Emacs."
     map "C-x C-x d f" (view-emacs-FAQ))
    (y/define-function-and-bind
     y/display-copying "Display copying of Emacs."
     map "C-x C-x d c" (describe-copying))
    map)
  ;; body
  )

(defun y/keymap-mode-on()
  "Enable y/keymap-mode if applicable for the buffer.
Disabled if `major-mode' of buffer is not the member of
list `y/keymap-major-modes-disabled'."
  (unless (member major-mode y/keymap-major-modes-disabled)
    (y/keymap-mode +1)))

(define-globalized-minor-mode
  y/keymap-global-mode y/keymap-mode y/keymap-mode-on)

(eval-and-compile
  (y/define-set-key-function
   y/keymap-set-key y/keymap-unset-key y/keymap-mode-map))

(y/keymap-global-mode +1)

(defun y/use-package-handler/:bind(args)
  "Use `y/keymap-mode-map' if no :map specified.
ARGS is the argument list."
  (let ((v (nth 2 args)))
    (when (and y/keymap-as-use-package-map
               (listp v) (not (eq (car v) :map)))
      (push 'y/keymap-mode-map v)
      (push :map v)
      (setf (nth 2 args) v))
    args))
(advice-add 'use-package-handler/:bind
            :filter-args #'y/use-package-handler/:bind)

(provide 'y-keymap)

;;; y-keymap.el ends here
