;;; init.el --- Main Emacs configuration file  -*- lexical-binding: t; -*-

;;; Preamble

;; To inhibit this message you MUST do this in init.el, MUST use ‘setq’,
;; and MUST write your login name as a string literal.  Thanks Emacs!
;;
;; The ‘eval’ is required in the case that this file is byte-compiled.
(if mm-humanwave-p
    (eval '(setq inhibit-startup-echo-area-message "thomasvoss"))
  (eval '(setq inhibit-startup-echo-area-message "thomas")))

;; Require helpers used by the rest of the config
(require 'mm-lib)


;;; Silent Native Compilation

(when (native-comp-available-p)
  (setopt
   native-comp-async-report-warnings-errors nil
   native-compile-prune-cache t))


;;; Package Management

(setopt
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
   (substitute-quotes
    ";; This is `%s'.  Use `%s' to evaluate and print results.\n\n")
   initial-major-mode
   (substitute-command-keys
    "\\<lisp-interaction-mode-map>\\[eval-print-last-sexp]"))
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
  (next-error-recenter '(4))            ; ‘center of window’
  (read-extended-command-predicate #'command-completion-default-include-p)
  (remote-file-name-inhibit-auto-save t)
  (remote-file-name-inhibit-delete-by-moving-to-trash t)
  (save-interprogram-paste-before-kill t)
  (user-full-name "Thomas Voss")
  (user-mail-address "mail@thomasvoss.com")
  :config
  (load custom-file :noerror)
  (add-hook 'text-mode-hook #'auto-fill-mode)
  (add-hook 'before-save-hook
            (defun mm-delete-final-newline ()
              (let ((end (point-max)))
                (unless (or require-final-newline
                            mode-require-final-newline
                            (not (= (char-before end) ?\n)))
                  (delete-region (1- end) end)))))
  (add-hook 'before-save-hook #'delete-trailing-whitespace)
  (prefer-coding-system 'utf-8))


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
  (bookmark-save-flag 0))


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

(require 'mm-abbrev)
(require 'mm-buffer-menu)
(require 'mm-calc)
(require 'mm-completion)
(require 'mm-dired)
(require 'mm-documentation)
(require 'mm-editing)
(require 'mm-keybindings)
(require 'mm-modeline)
(require 'mm-projects)
(require 'mm-search)
(require 'mm-tetris)
(require 'mm-theme)
(require 'mm-window)
(when (eq system-type 'darwin)
  (require 'mm-darwin))
(when mm-humanwave-p
  (require 'mm-humanwave))
(when (treesit-available-p)
  (require 'mm-treesit))


;;; Postamble

(add-hook 'after-init-hook
          (defun mm-echo-init-time ()
            (message (emacs-init-time "Emacs initialized in %.2f seconds")))
          100)
