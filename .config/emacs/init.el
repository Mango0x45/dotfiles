;;; init.el --- Main Emacs configuration file  -*- lexical-binding: t; -*-

;;; Preamble

;; To inhibit this message you must do this in init.el (not
;; early-init.el!), you must use ‘setq’ (not ‘setopt’!), and you must
;; write your login name as a string (you shan’t use ‘user-login-name’!).
;; Lord knows why this needs to be so complicated…
;;
;; The ‘eval’ is required in the case that this file is byte-compiled.
(if mm-humanwave-p
    (eval '(setq inhibit-startup-echo-area-message "thomasvoss"))
  (eval '(setq inhibit-startup-echo-area-message "thomas")))

;; Add all my custom lisp code into the load path
(dolist (directory
         (list mm-config-directory
               (expand-file-name "modules" mm-config-directory)
               (expand-file-name "site-lisp" mm-config-directory)))
  (add-to-list 'load-path directory))


;;; Disable or Enable LSP?

;; I’m not decided on LSP… so make it a variable

(defvar mm-lsp-p nil
  "Enable LSP support if non-nil.")


;;; Convenience Macros and -Functions

(defun mm-mode-to-hook (mode)
  "Get the hook corresponding to MODE."
  (declare (ftype (function (symbol) symbol))
           (pure t) (side-effect-free t))
  (intern (concat (symbol-name mode) "-hook")))

(defun mm-mode-to-ts-mode (mode)
  "Get the Tree-Sitter mode corresponding to MODE."
  (declare (ftype (function (symbol) symbol))
           (pure t) (side-effect-free t))
  (intern (concat
           (string-remove-suffix "-mode" (symbol-name mode))
           "-ts-mode")))

(defun mm-ts-mode-to-mode (ts-mode)
  "Get the non-Tree-Sitter mode corresponding to TS-MODE."
  (declare (ftype (function (symbol) symbol))
           (pure t) (side-effect-free t))
  (intern (concat
           (string-remove-suffix "-ts-mode" (symbol-name ts-mode))
           "-mode")))

(defsubst mm-string-split (separators string)
  "Split STRING on SEPARATORS.
Wrapper around `string-split' that puts separators first.  This makes it
convenient to use in `thread-last'."
  (declare (ftype (function (string string) (list string)))
           (pure t) (side-effect-free t))
  (string-split string separators))

(defun mm-as-number (string-or-number)
  "Ensure STRING-OR-NUMBER is a number.
If given a number return STRING-OR-NUMBER as-is, otherwise convert it to
a number and then return it.

This function is meant to be used in conjuction with `read-string' and
`format-prompt'."
  (declare (ftype (function (or string number) number))
           (pure t) (side-effect-free t))
  (if (stringp string-or-number)
      (string-to-number string-or-number)
    string-or-number))

(defun mm-do-and-center (function &rest arguments)
  "Call FUNCTION with ARGUMENTS and then center the screen."
  (apply function arguments)
  (when (called-interactively-p)
    (recenter)))

(defmacro mm-comment (&rest _body)
  "Comment out BODY.  A cleaner alternative to line-commenting a region."
  (declare (indent 0))
  nil)

(defun mm-nil (&rest _)
  "Return nil."
  nil)

(defmacro mm-with-suppressed-output (&rest body)
  "Execute BODY while suppressing output.
Execute BODY as given with all output to the echo area or the *Messages*
buffer suppressed."
  (declare (indent 0))
  `(let ((inhibit-message t)
         (message-log-max nil))
     ,@body))

(defun mm-rotate-left (n list)
  "Rotate the elements of LIST N places to the left."
  (declare (ftype (function (number (list t)) (list t)))
           (pure t) (side-effect-free t))
  (append (nthcdr n list) (butlast list (- (length list) n))))

(defun mm-rotate-right (n list)
  "Rotate the elements of LIST N places to the right."
  (declare (ftype (function (number (list t)) (list t)))
           (pure t) (side-effect-free t))
  (mm-rotate-left (- (length list) n) list))


;;; Silent Native Compilation

(when (native-comp-available-p)
  (setopt
   native-comp-async-report-warnings-errors nil
   native-compile-prune-cache t))


;;; Package Management

(setopt
 package-vc-register-as-project nil
 package-user-dir (expand-file-name "pkg" mm-data-directory)
 package-gnupghome-dir (or (getenv "GNUPGHOME")
                           (expand-file-name "gnupg" package-user-dir))
 package-archives (cl-loop with proto = (if (gnutls-available-p) "https" "http")
                           for (name . url) in
                             '(("gnu"    . "elpa.gnu.org/packages/")
                               ("melpa"  . "melpa.org/packages/")
                               ("nongnu" . "elpa.nongnu.org/nongnu/"))
                           collect (cons name (concat proto "://" url)))
 package-archive-priorities '(("gnu"    . 3)
                              ("nongnu" . 2)
                              ("melpa"  . 1)))
(setopt use-package-always-defer t)

(package-initialize)

(defun mm-package-sync ()
  "Remove unused packages and install missing ones."
  (interactive)
  (let ((window-configuration (current-window-configuration)))
    (package-autoremove)
    (package-install-selected-packages)
    (package-upgrade-all)
    (package-vc-install-selected-packages)
    (package-vc-upgrade-all)
    (set-window-configuration window-configuration))
  (message "Done syncing packages."))


;;; Generic Emacs Configuration

(defvar mm-initial-scratch-message
  (format
   ";; This is `%s'.  Use `%s' to evaluate and print results.\n\n"
   initial-major-mode
   (propertize
    (substitute-command-keys
     "\\<lisp-interaction-mode-map>\\[eval-print-last-sexp]")))
  "The initial message to display in the scratch buffer.")

(use-package emacs
  :demand t
  :custom
  (ad-redefinition-action 'accept)
  (case-fold-search nil)
  (create-lockfiles nil)
  (custom-file (expand-file-name "custom.el" mm-config-directory))
  (custom-safe-themes t)
  (delete-pair-blink-delay 0)
  (disabled-command-function nil)
  (duplicate-line-final-position -1)
  (duplicate-region-final-position -1)
  (echo-keystrokes 0.01)                ; 0 disables echoing
  (echo-keystrokes-help nil)
  (extended-command-suggest-shorter nil)
  (initial-buffer-choice nil)
  (initial-scratch-message mm-initial-scratch-message)
  (kill-do-not-save-duplicates t)
  (large-file-warning-threshold nil)
  (make-backup-files nil)
  (mode-require-final-newline nil)
  (next-error-recenter '(4))            ; ‘center of window’
  (read-extended-command-predicate #'command-completion-default-include-p)
  (remote-file-name-inhibit-auto-save t)
  (remote-file-name-inhibit-delete-by-moving-to-trash t)
  (require-final-newline mm-humanwave-p)
  (save-interprogram-paste-before-kill t)
  (user-full-name "Thomas Voss")
  (user-mail-address "mail@thomasvoss.com")
  :config
  (load custom-file :noerror)
  (setq-default fill-column 80)
  (dolist (mode '(text-mode emacs-lisp-mode lisp-mode))
    (add-hook (mm-mode-to-hook mode)
              (defun mm-set-fill-column ()
                (setq-local fill-column 73))))
  (add-hook 'text-mode-hook #'auto-fill-mode)
  (add-hook 'before-save-hook
            (defun mm-delete-final-newline ()
              (let ((end (point-max)))
                (unless (or require-final-newline
                            mode-require-final-newline
                            (not (= (char-before end) ?\n)))
                  (delete-region (1- end) end)))))
  (add-hook 'before-save-hook #'delete-trailing-whitespace)
  (prefer-coding-system 'utf-8)

  ;; Disabled modes
  (blink-cursor-mode -1)
  (line-number-mode -1)
  (tooltip-mode -1))


;;; Instantly highlight matching parens

(use-package paren
  :custom
  (show-paren-delay 0))


;;; Auto Revert Buffers

(use-package autorevert
  :custom
  (global-auto-revert-non-file-buffers t)
  :init
  (add-hook
   'after-change-major-mode-hook
   (defun mm-enable-autorevert ()
     (unless (derived-mode-p 'Buffer-menu-mode)
       (auto-revert-mode)))))


;;; Bookmarks

(use-package bookmark
  :custom
  (bookmark-save-flag 1))


;;; Automatically Create- and Delete Directories

(defun mm-auto-create-directories (function filename &rest arguments)
  "Automatically create and delete parent directories of files.
This is an `:override' advice for `find-file' and friends.  It
automatically creates the parent directories of the file being visited
if necessary.  It also sets a buffer-local variable so that the user
will be prompted to delete the newly created directories if they kill
the buffer without saving it."
  (let (dirs-to-delete)
    (let* ((dir-to-create (file-name-directory filename))
           (current-dir dir-to-create))
      ;; Add each directory component to ‘dirs-to-delete’
      (while (not (file-exists-p current-dir))
        (push current-dir dirs-to-delete)
        (setq current-dir (file-name-directory
                           (directory-file-name current-dir))))
      (unless (file-exists-p dir-to-create)
        (make-directory dir-to-create :parents)))
    (prog1
        (apply function filename arguments)
      (when dirs-to-delete
        (setq-local mm-find-file--dirs-to-delete (reverse dirs-to-delete))
        (add-hook 'kill-buffer-hook #'mm-find-file--maybe-delete-directories
                  :depth :local)
        (add-hook 'after-save-hook  #'mm-find-file--remove-hooks
                  :depth :local)))))

(defun mm-find-file--maybe-delete-directories ()
  (unless (file-exists-p buffer-file-name)
    (dolist (directory mm-find-file--dirs-to-delete)
      (when (and (stringp directory)
                 (file-exists-p directory)
                 (thread-last
                   (directory-file-name directory)
                   (format "Also delete directory `%s'?")
                   (substitute-quotes)
                   (y-or-n-p)))
        (delete-directory directory)))))

(defun mm-find-file--remove-hooks ()
  (remove-hook 'kill-buffer-hook
               #'mm-find-file--maybe-delete-directories
               :local)
  (remove-hook 'after-save-hook
               #'mm-find-file--remove-hooks
               :local))

(dolist (command #'(find-file find-alternate-file write-file))
  (advice-add command :around #'mm-auto-create-directories))


;;; Load Modules

(require 'mm-abbrev)                    ; Text Expansion
(require 'mm-buffer-menu)               ; Buffer Menu
(require 'mm-calc)                      ; Emacs Calc
(require 'mm-completion)                ; Completions
(require 'mm-dired)                     ; Dired
(require 'mm-documentation)             ; Documentation
(require 'mm-editing)                   ; Text Editing
(require 'mm-keybindings)               ; Keybindings
(require 'mm-modeline)                  ; Modeline
(require 'mm-org)                       ; Org-Mode
(require 'mm-projects)                  ; Project Management
(require 'mm-search)                    ; Text Searching
(require 'mm-spellcheck)                ; Spell Checking
(require 'mm-tetris)                    ; Emacs Tetris
(require 'mm-theme)                     ; Themeing
(require 'mm-treesit)                   ; Tree-Sitter
(require 'mm-window)                    ; Windowing
(when mm-darwin-p
  (require 'mm-darwin))                 ; MacOS
(when mm-lsp-p
  (require 'mm-lsp))                    ; Language Server Protocol


;;; Postamble

(add-hook 'after-init-hook
          (defun mm-echo-init-time ()
            (message (emacs-init-time "Emacs initialized in %.2f seconds")))
          100)