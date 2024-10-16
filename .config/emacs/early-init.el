;;; early-init.el --- Emacs early init file  -*- lexical-binding: t; -*-

;;; XDG Base Directory Specification Compliance
(defconst mm-cache-directory
  (expand-file-name
   "emacs"
   (or (getenv "XDG_CACHE_HOME")
       (expand-file-name ".cache" (getenv "HOME"))))
  "The XDG-conformant cache directory that Emacs should use.")

(defconst mm-config-directory
  (expand-file-name
   "emacs"
   (or (getenv "XDG_CONFIG_HOME")
       (expand-file-name ".config" (getenv "HOME"))))
  "The XDG-conformant config directory that Emacs should use.")

(defconst mm-data-directory
  (expand-file-name
   "emacs"
   (or (getenv "XDG_DATA_HOME")
       (expand-file-name ".local/share" (getenv "HOME"))))
  "The XDG-conformant data directory that Emacs should use.")

(mapc (lambda (directory)
        (make-directory directory :parents))
      (list mm-cache-directory mm-config-directory mm-data-directory))

(setopt user-emacs-directory mm-cache-directory
        auto-save-list-file-prefix (expand-file-name
                                    "auto-save-list-"
                                    mm-cache-directory)
        backup-directory-alist `(("." . ,(expand-file-name
                                          "backups" mm-cache-directory))))
(when (native-comp-available-p)
  (startup-redirect-eln-cache
   (expand-file-name (expand-file-name "eln/" mm-cache-directory))))


;;; Useful Constants

(defconst mm-darwin-p (eq system-type 'darwin)
  "This variable is non-nil if Emacs is running on a Darwin system.")


;;; Basic Frame Settings

(setopt frame-resize-pixelwise t
        frame-inhibit-implied-resize t
        ring-bell-function #'ignore
        use-short-answers t
        inhibit-splash-screen t
        inhibit-startup-buffer-menu t)
(if mm-darwin-p
    (progn
      (add-to-list 'default-frame-alist '(fullscreen . maximized))
      (when (featurep 'ns)
        (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))))
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
;; temporarily and restore them over Emacs has properly initialized.  We
;; set threshold to 8 MiB which seems to be a good middleground for now.
;; A higher threshold means less garbage collections but I’ve had issues
;; with those garbage collections causing long freezes when they occur.
(let ((saved-file-name-handler-alist file-name-handler-alist)
      (saved-vc-handled-backends vc-handled-backends))
  (setopt file-name-handler-alist nil
          vc-handled-backends nil)
  (add-hook
   'emacs-startup-hook
   (defun mm-restore-emacs-settings ()
     (setopt gc-cons-threshold (* 1024 1024 8)
             gc-cons-percentage 0.1
             file-name-handler-alist saved-file-name-handler-alist
             vc-handled-backends saved-vc-handled-backends))))


;;; Avoid Flashbang

(setq-default mode-line-format nil) ; This will be set in init.el

;; Colors taken from ‘mango-theme’
(let ((background "#2B303B")
      (foreground "#C5C8C6"))
      (set-face-attribute
       'default nil
       :background background
       :foreground foreground)
      (set-face-attribute
       'mode-line nil
       :background background
       :foreground foreground
       :box 'unspecified))
