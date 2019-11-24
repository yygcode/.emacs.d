;;; y-auxiliary.el --- Misc Auxiliary Routines

;; Copyright (C) 2018 yanyg<yygcode@gmail.com>

;; Author: yanyg<yygcode@gmail.com>
;; Maintainer: yanyg<yygcode@gmail.com>
;; Keyword: Emacs Initialization Customization Configuration
;; URL: https://ycode.org; http://ycode.org

;;; Commentary:

;; Common Routines

;;; Code:

(require 'ielm)
(require 'derived)

(require 'eldoc)
(require 'elisp-mode)
(require 'lisp-mode)
(require 'ielm)

(defmacro y/inc(n)
  "Increment N."
  `(setq ,n (1+ ,n)))

;; language enhancement
(defmacro y/for(initial condition change &rest body)
  "Loop if condition is satisfied.
Execute INITIAL, then check CONDITION, after BODY is executed, do CHANGE,
and then check CONDITION again."
  `(let ,initial
     (while ,condition
       ,@body
       ,change)))

(defmacro y/count-loop(var from to &optional inc &rest body)
  "Loop from FROM to TO, execute BODY and INC VAR each time."
  `(y/for ((,var ,from)) ;; initial
          (< ,var ,to) ;; condition
          (,(or inc 'y/inc) ,var) ;; change
          ,@body))

;; integer/math
(defmacro y/nzerop(arg)
  "Check whether ARG is zero number.
Returns t if is zero number, otherwise return nil"
  `(not (zerop ,arg)))

(defmacro y/delete(elt seq)
  "Delete ELT in SEQ, and restore the result to SEQ.
Use `delete' to remove from SEQ."
  `(setq ,seq (delete ,elt ,seq)))

(defmacro y/first-non-nil(&rest args)
  "Return first non nil in ARGS."
  (catch 'final
    (dolist (l args)
      (when l
        (throw 'final l)))))

(defmacro y/file-replace-extension(filename extension)
  "Replace FILENAME suffix(extension) to EXTENSION."
  `(concat (file-name-sans-extension ,filename) "." ,extension))

(defmacro y/template-find-file(filename)
  "Generate a function Y/FIND-FILE--FILENAME which is used to open FILENAME."
  `(defun ,(intern (format "y/find-file--%s" filename)) ()
     ,(format "Call `find-file' `%s'." filename)
     (interactive)
     (find-file ,filename)))

(defmacro y/template-find-file-and-bind(filename map keyseq)
  "Generate a `find-file' FILENAME function and bind to KEYSEQ in MAP.
Use `global-map' if MAP is nil."
  `(define-key
     (y/first-non-nil ,map global-map) ,(kbd keyseq)
     (y/template-find-file ,filename)))

(defmacro y/template-switch-to-buffer(buffername &rest body)
  "Generate a function Y/SWITCH-TO--BUFFERNAME to switch to BUFFERNAME.
Eval BODY if create a new buffer and BODY is non nil."
  `(defun ,(intern (format "y/switch-to--%s" buffername)) ()
     ,(format "Call `switch-to-buffer' `%s'.\nEval %s" buffername
              body)
     (interactive)
     (let ((newcreate (not (get-buffer ,buffername))))
      (when (and (switch-to-buffer ,buffername) newcreate)
        ,@body))))

(defmacro y/template-switch-to-buffer-and-bind(buffername map keyseq &rest body)
  "Generate a switch to BUFFERNAME function and bind to KEYSEQ in MAP.
Use `global-map' if MAP is nil.  Eval BODY if non nil."
  `(define-key
     (y/first-non-nil ,map global-map) ,(kbd keyseq)
     (y/template-switch-to-buffer ,buffername ,@body)))

(defmacro y/template-kill-buffer(buffername)
  "Generate a function Y/KILL-BUFFER--BUFFERNAME to kill BUFFERNAME."
  `(defun ,(intern (format "y/switch-to--%s" buffername)) ()
     ,(format "Call `kill-buffer' `%s'." buffername)
     (interactive)
     (save-excursion
       (and (switch-to-buffer ,buffername)
            (kill-buffer)))))

(defmacro y/template-kill-buffer-and-bind(buffername map keyseq)
  "Generate a `kill-buffer' BUFFERNAME function and bind to KEYSEQ in MAP.
Use `global-map' if MAP is nil."
  `(define-key
     (y/first-non-nil ,map global-map) ,(kbd keyseq)
     (y/template-kill-buffer ,buffername)))

(defmacro y/template-generate-function(func doc &rest body)
  "Generate a function FUNC with docstring DOC and body BODY."
  `(defun ,func ()
     ,doc
     (interactive)
     ,@body))

(defmacro y/template-generate-function-and-bind(func doc map keyseq &rest body)
  "Generate a function FUNC with DOC and BODY, then bind to KEYSEQ in MAP."
    `(define-key
       (y/first-non-nil ,map global-map) ,(kbd keyseq)
       (y/template-generate-function ,func ,doc ,@body)))

(defmacro y/template-lambda-bind(map keyseq &rest body)
  "Bind KEYSEQ to MAP, use `global-map' if MAP is nil.  BODY is lambda body."
  `(define-key
     (y/first-non-nil ,map global-map) ,(kbd keyseq)
     #'(lambda()(interactive) ,@body)))

(defun y/delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun y/backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (y/delete-word (- arg)))

(defun y/comment-or-uncomment()
  "Call `comment-or-uncomment-region'.
Use current line if no active region."
  (interactive)
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position)
                                 (line-end-position))))

;; lisp mode
(defconst y/lisp-modes
  '(emacs-lisp-mode lisp-mode ielm-mode lisp-interaction-mode)
  "Major modes relate to LISP.")

(defvar y/lisp-modes-hook nil
  "Hook for all LISP mode.")

(defun y/lisp-modes--hook-function()
  "Run `y/lisp-mode-hook' for all LISP mode."
  (run-hooks 'y/lisp-modes-hook))

(dolist (hook (mapcar #'derived-mode-hook-name y/lisp-modes))
  (add-hook hook 'y/lisp-modes--hook-function))

(defmacro y/template-lisp-modes-foreach(func)
  "Execute FUNC with argument mode for each LISP mode."
  `(dolist (mode y/lisp-modes)
     (funcall ,func mode)))

(defun y/add-after-init-or-make-frame-hook(func)
  "Add FUNC to after-init or after-make-frame hook depends on daemon mode."
  (if (daemonp)
      (add-hook 'after-make-frame-functions func)
    (add-hook 'after-init-hook func)))

;; quelpa self update
(defun y/upgrade-quelpa()
  "Upgrade quelpa package."
  (interactive)
  (unless (package-installed-p 'quelpa)
    (user-error "Package `quelpa' does not installed"))
  (require 'quelpa)
  (quelpa-self-upgrade)
  (quelpa-upgrade)
  (quelpa-checkout-melpa)
  (with-temp-buffer
    (url-insert-file-contents
     "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))

(defun y/date(&optional insert)
  "Show Today Date in echo area.  Insert to current buffer if INSERT."
  (interactive "P")
  (message (format-time-string "%Y-%m-%d"))
  (and insert
       (if buffer-read-only
           (error "Could not insert to read-only buffer")
         (insert (format-time-string "%Y-%m-%d")))))

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun y/rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;; source: https://emacs.stackexchange.com/a/24461
(defun y/revert-all-file-buffers()
  "Refresh all open file buffers without confirmation.
Buffers in modified (not yet saved) state in Emacs will not be reverted.
They will be reverted though if they were modified outside Emacs.
Buffers visiting files which do not exist any more or are no longer readable
will be killed."
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      ;; Revert only buffers containing files, which are not modified;
      ;; do not try to revert non-file buffers like *Messages*.
      (when (and filename
                 (not (buffer-modified-p buf)))
        (if (file-readable-p filename)
            ;; If the file exists and is readable, revert the buffer.
            (with-current-buffer buf
              (revert-buffer :ignore-auto :noconfirm :preserve-modes))
          ;; Otherwise, kill the buffer.
          (let (kill-buffer-query-functions) ; No query done when killing buffer
            (kill-buffer buf)
            (message "Killed non-existing/unreadable file buffer: %s" filename))))))
  (message "Finished reverting buffers containing unmodified files."))

(defun y/view-lossage-timer()
  "Show `view-lossage' periodically."
  (interactive)
  (defvar y/--view-lossage-timer nil)
  (if y/--view-lossage-timer
      (progn
        (cancel-timer y/--view-lossage-timer)
        (setq y/--view-lossage-timer nil))
    (setq y/--view-lossage-timer (run-at-time nil .5 #'view-lossage))))

;; env process
(defun y/env-set(env val &optional force)
  "Set enviroment ENV to VAL if ENV unset or FORCE is t."
  (when (or (not (getenv env)) force)
    (setenv env val)))
(defun y/env-sync-partner(env1 env2)
  "Sync enviroment ENV1/ENV2 to another if one is nil and another is non-nil."
  (or (getenv env1) (y/env-set env1 (getenv env2)))
  (or (getenv env2) (y/env-set env2 (getenv env1))))

(defun y/add-no-proxy-sites(&rest sites)
  "Add SITES to environment `no_proxy'."
  (let* ((no-proxy-sites
          (let ((env (getenv "no_proxy")))
            (and env (split-string env ",")))))
    (cl-labels
        ((add-sites (sites)
          (dolist (s sites)
            (pcase s
              ('nil t)
              ((pred stringp)
               (and (> (length s) 0)
                    (add-to-list 'no-proxy-sites s t)))
              ((pred listp)
               (funcall add-sites (car s))
               (funcall add-sites (cdr s)))
              (_ (warn "Unknown type(%s) object: %S" (type-of s) s))))
          no-proxy-sites))
      (add-sites sites))
    (y/env-set "no_proxy" (mapconcat 'identity no-proxy-sites ",") t)))

(provide 'y-auxiliary)

;;; y-auxiliary.el ends here
