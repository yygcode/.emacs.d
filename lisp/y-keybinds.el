;;; y-keybinds.el --- Customized Keybinds

;; Copyright (C) 2018 yanyg<yygcode@gmail.com>

;; Author: yanyg<yygcode@gmail.com>
;; Maintainer: yanyg<yygcode@gmail.com>
;; Keyword: Emacs Initialization Customization Configuration
;; URL: https://ycode.org

;; SPDX-License-Identifier: GPL-2.0-or-later

;;; Commentary:

;; Keybind Conventions
;; Reference from Manual
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html
;; DO NOT Rules:
;; 1. Don't bind C-h following any prefix character; If you don't bind C-h,
;;    it is automatically available as a help character for listing the
;;    subcommands of the prefix character;
;; 2. Don't bind a key sequence ending in C-g, since that is commonly used to
;;    cancel a key sequence;
;; 3. Don't bind a key sequence ending in <ESC> except following another <ESC>;

;; Important Extension:
;; 1. C-x C-x: Reserved to provide more extension;
;;    `exchange-dot-and-mark' binds to 'C-x C-x C-x';
;; 2. C-o: Used to `other-window'; `open-line' binds to 'C-x C-x';
;; 3. Enhance keyboard: <f1>-<f9>: C-x C-x [1-9];

;; Common Routines

;;; Code:

(require 'y-auxiliary)

(defun y/overriding-map-alist-by-basic-keybind(&optional arg)
  "Enable or disable overriding map alist by Y/BASIC-KEYBIND depends on ARG.
Disable if ARG is negative, enable if ARG is positive, otherwise toggle it."
  (interactive)
  (let ((bkm (cons 'y/basic-keybind-mode
                   y/basic-keybind-mode-map)))
    (unless (and arg (integerp arg) (y/nzerop arg))
      (setq arg
            (if (member bkm minor-mode-overriding-map-alist)
                -1
              1)))
    (if (equal arg -1)
        (y/delete bkm minor-mode-overriding-map-alist)
      (add-to-list 'minor-mode-overriding-map-alist bkm)))
  (when (called-interactively-p 'interactive)
    (message "%s y/basic-keybind overriding map."
             (if (< 0 arg) "Enable" "Disable"))))

(defvar y/basic-keybind-non-overriding-major-modes
  '(minibuffer-inactive-mode)
  "No y/basic-keybind overriding map major modes list.")

(define-minor-mode y/basic-keybind-mode "Basic keybind"
  :lighter " y-basic-keybind"
  :init-value t
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
    (define-key map (kbd "C-c C-c") #'y/comment-or-uncomment)
    (define-key map (kbd "M-d") #'y/delete-word)
    (define-key map (kbd "M-<backspace>") #'y/backward-delete-word)
    (define-key map (kbd "M-<DEL>") #'y/backward-delete-word)
    (define-key map (kbd "C-x C-x =") #'describe-char)

    ;; quick visit
    (y/template-switch-to-buffer-and-bind "*Help*" map "C-c q h")
    (y/template-switch-to-buffer-and-bind "*scratch*" map "C-c q s"
                                          (insert initial-scratch-message)
                                          (set-buffer-modified-p nil))
    (y/template-find-file-and-bind user-init-file map "C-c q i")
    (y/template-find-file-and-bind y/user-init-config map "C-c q c")
    (y/template-find-file-and-bind y/lisp-directory map "C-c q d")
    (y/template-find-file-and-bind
     (y/file-replace-extension y/user-init-config "el")
     map "C-c q C")
    (y/template-generate-function-and-bind
     y/reload-init-file "Reload Emacs init.el"
     map "C-c q R" (load-file user-init-file))

    (y/template-kill-buffer-and-bind "*Help*" map "C-x C-x k h")

    ;; Emacs helper buffers.
    (y/template-generate-function-and-bind
     y/display-startup-screen "Display startup screen."
     map "C-x C-x d s" (display-startup-screen))
    (y/template-generate-function-and-bind
     y/display-about-screen "Display about screen."
     map "C-x C-x d a" (display-about-screen))
    (y/template-generate-function-and-bind
     y/display-todo "Display TODO of Emacs."
     map "C-x C-x d t" (view-emacs-todo t))
    (y/template-generate-function-and-bind
     y/display-faq "Display FAQ of Emacs."
     map "C-x C-x d f" (view-emacs-FAQ))
    (y/template-generate-function-and-bind
     y/display-copying "Display copying of Emacs."
     map "C-x C-x d c" (describe-copying))
    map)
  ;; body
  (unless (member major-mode y/basic-keybind-non-overriding-major-modes)
      (cond ((and y/basic-keybind-mode (featurep 'y-keybinds))
             ;; ensure basic keybind precedence/order.
             (y/overriding-map-alist-by-basic-keybind 1))
            (t
             (y/overriding-map-alist-by-basic-keybind -1)))))

(defun y/basic-keybind-mode-on()
  "Active y/basic-keybind-mode."
  (interactive)
  (y/basic-keybind-mode 1))

(define-globalized-minor-mode y/basic-keybind-global-mode
  y/basic-keybind-mode y/basic-keybind-mode-on)

(y/basic-keybind-global-mode 1)

(dolist (hook '(minibuffer-inactive-mode-hook))
  (add-hook hook
            #'(lambda()
                "Diable y/basic-keybind overriding map."
                (y/overriding-map-alist-by-basic-keybind -1))))

(when (featurep 'use-package)
  ;; use y/basick-keybind-mode-map as default map.
  (defun y/use-package-handler/:bind(args)
    "Use `y/basic-keybind-mode-map' if no :map specified."
    (let ((v (nth 2 args)))
      (when (and (listp v) (not (eq (car v) :map)))
        (push 'y/basic-keybind-mode-map v)
        (push :map v)
        (setf (nth 2 args) v))
      args))
  (advice-add 'use-package-handler/:bind
              :filter-args #'y/use-package-handler/:bind))

(defun y/basic-set-key(key command)
  "Give KEY a `y/basic-keybind-mode' binding as COMMAND.
Like as `global-set-key' but use y/basic-keybind-mode-map."
  (interactive
   (let* ((menu-prompting nil)
          (key (read-key-sequence "Set key in y/basic-keybind: ")))
     (list key
           (read-command (format "Set key %s to command: "
                                 (key-description key))))))
  (or (vectorp key) (stringp key)
      (signal 'wrong-type-argument (list 'arrayp key)))
  (define-key y/basic-keybind-mode-map key command))

(defun y/basic-unset-key (key)
  "Remove a `y/basic-keybind-mode' binding of KEY.
KEY is a string or vector representing a sequence of keystrokes."
  (interactive "kUnset key : ")
  (y/basic-set-key key nil))

(provide 'y-keybinds)

;;; y-keybinds.el ends here
