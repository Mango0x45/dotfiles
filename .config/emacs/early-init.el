;;; early-init.el --- Emacs early init file  -*- lexical-binding: t; -*-

;;; XDG Base Directory Specification Compliance

(eval-when-compile
  (require 'xdg))

(defconst mm-cache-directory
  (expand-file-name "emacs/" (xdg-cache-home))
  "The XDG-conformant cache directory that Emacs should use.")

(defconst mm-config-directory
  (expand-file-name "emacs/" (xdg-config-home))
  "The XDG-conformant config directory that Emacs should use.")

(defconst mm-data-directory
  (expand-file-name "emacs/" (xdg-data-home))
  "The XDG-conformant data directory that Emacs should use.")

(dolist (directory (list mm-cache-directory
                         mm-config-directory
                         mm-data-directory))
  (make-directory directory :parents))

(setopt user-emacs-directory (concat mm-cache-directory "/")
        auto-save-list-file-prefix (expand-file-name
                                    "auto-save-list-"
                                    mm-cache-directory)
        backup-directory-alist `(("." . ,(expand-file-name
                                          "backups" mm-cache-directory))))

(when (native-comp-available-p)
  (startup-redirect-eln-cache
   (expand-file-name (expand-file-name "eln/" mm-cache-directory))))


;;; Useful Constants

(defconst mm-humanwave-p (file-exists-p "~/.humanwavep")
  "This variable is non-nil if Emacs is running on a Humanwave system.")


;;; Basic Frame Settings

(setopt frame-resize-pixelwise t
        frame-inhibit-implied-resize t
        ring-bell-function #'ignore
        use-short-answers t
        inhibit-splash-screen t
        inhibit-startup-buffer-menu t)
(if (eq system-type 'darwin)
    (progn
      (add-to-list 'default-frame-alist '(fullscreen . maximized))
      (when (featurep 'ns)
        (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))))
  (add-to-list 'default-frame-alist '(undecorated . t))
  (menu-bar-mode -1))
(scroll-bar-mode -1)
(tool-bar-mode -1)


;;; Startup Performance

(setopt gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.5)
(setopt read-process-output-max
        (let ((pipe-size-file "/proc/sys/fs/pipe-max-size"))
          (if (file-exists-p pipe-size-file)
              (with-temp-buffer
                (insert-file-contents pipe-size-file)
                (number-at-point))
            (* 1024 1024))))

;; Set ‘file-name-handler-alist’ and ‘vc-handled-backends’ to nil
;; temporarily and restore them once Emacs has properly initialized.
;; We set threshold to 8 MiB which seems to be a good middleground for
;; now.  A higher threshold means less garbage collections but I’ve
;; had issues with those garbage collections causing long freezes when
;; they occur.
(let ((saved-file-name-handler-alist file-name-handler-alist))
  (setopt file-name-handler-alist nil)
  (add-hook
   'emacs-startup-hook
   (defun mm-restore-emacs-settings ()
     (setopt gc-cons-threshold (* 1024 1024 8)
             gc-cons-percentage 0.1
             file-name-handler-alist saved-file-name-handler-alist))))


;;; Set Load Paths

(dolist (directory '("." "modules" "user-lisp"))
  (add-to-list 'load-path (expand-file-name directory mm-config-directory)))
(setopt custom-theme-directory (expand-file-name "themes" mm-config-directory))


;;; Set Theme

(defun mm-dark-p ()
  (cond
   ((eq system-type 'gnu/linux)
    (string-match-p
     "prefer-dark"
     (shell-command-to-string
      "gsettings get org.gnome.desktop.interface color-scheme 2>/dev/null")))
   ((eq system-type 'darwin)
    (if (boundp 'ns-system-appearance)
        (eq ns-system-appearance 'dark)
      (string-match-p
       "Dark"
       (shell-command-to-string
        "defaults read -g AppleInterfaceStyle 2>/dev/null"))))))

(load-theme (if (mm-dark-p) 'mango-dark 'mango-light) :no-confirm)
