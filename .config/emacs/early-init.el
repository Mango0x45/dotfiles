;;; early-init.el --- Emacs early init file  -*- lexical-binding: t; -*-

(defmacro x-set (&rest body)
  (declare (indent 0))
  (unless (zerop (% (length body) 2))
    (error "Uneven number of variable+value pairs"))
  (macroexp-progn
   (mapcar
    (lambda (pair)
      `(customize-set-variable ,(macroexp-quote (car pair)) ,(cadr pair)))
    (seq-split body 2))))

(defconst 1-KiB 1024
  "The number of bytes in 1 kibibyte")

(defconst 1-MiB (* 1-KiB 1024)
  "The number of bytes in 1 mebibyte.")

(defconst 1-GiB (* 1-MiB 1024)
  "The number of bytes in 1 gibibyte.")

(defconst x-cache-directory
  (expand-file-name
   "emacs"
   (or (getenv "XDG_CACHE_HOME")
       (expand-file-name ".cache" (getenv "HOME"))))
  "The XDG-conformant cache directory that Emacs should use.")

(defconst x-config-directory
  (expand-file-name
   "emacs"
   (or (getenv "XDG_CONFIG_HOME")
       (expand-file-name ".config" (getenv "HOME"))))
  "The XDG-conformant config directory that Emacs should use.")

(defconst x-data-directory
  (expand-file-name
   "emacs"
   (or (getenv "XDG_DATA_HOME")
       (expand-file-name ".local/share" (getenv "HOME"))))
  "The XDG-conformant data directory that Emacs should use.")

;; Create standard Emacs directories
(dolist (dir (list x-cache-directory
                   x-config-directory
                   x-data-directory))
  (make-directory dir 'parents))

(x-set
  user-emacs-directory x-cache-directory
  auto-save-list-file-prefix (expand-file-name
                              "auto-save-list/" x-cache-directory)
  backup-directory-alist `(("." . ,(expand-file-name
                                    "backups" x-cache-directory))))

(when (native-comp-available-p)
  (startup-redirect-eln-cache
   (expand-file-name (expand-file-name "eln/" x-cache-directory))))

;; Temporarily set some variables to improve startup performance.  We
;; undo this in a following hook
(let ((saved-file-name-handler-alist file-name-handler-alist)
      (saved-vc-handled-backends     vc-handled-backends))
  (x-set
    gc-cons-threshold most-positive-fixnum
    gc-cons-percentage .5
    file-name-handler-alist nil
    vc-handled-backends nil)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (x-set
                gc-cons-threshold (* 8 1-MiB)
                gc-cons-percentage 0.1
                file-name-handler-alist saved-file-name-handler-alist
                vc-handled-backends saved-vc-handled-backends))))

(x-set read-process-output-max
       (let ((pipe-size-file "/proc/sys/fs/pipe-max-size"))
         (if (file-exists-p pipe-size-file)
             (with-temp-buffer
               (insert-file-contents pipe-size-file)
               (number-at-point))
           1-MiB)))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(x-set
  frame-resize-pixelwise t
  frame-inhibit-implied-resize t
  frame-title-format '("%b")
  ring-bell-function #'ignore
  use-dialog-box t
  use-file-dialog nil
  use-short-answers t
  inhibit-splash-screen t
  inhibit-startup-screen t
  inhibit-x-resources t
  inhibit-startup-echo-area-message user-login-name
  inhibit-startup-buffer-menu t)

;; Avoid the initial flash of white light when starting emacs
(setq mode-line-format nil)
(set-face-attribute 'default   nil
                    :background "#2B303B"
                    :foreground "#C5C8C6")
(set-face-attribute 'mode-line nil
                    :background "#2B303B"
                    :foreground "#C5C8C6"
                    :box 'unspecified)
