;; -*- lexical-binding: t; -*-

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

(setq user-emacs-directory x-cache-directory
      auto-save-list-file-prefix (expand-file-name
                                  "auto-save-list/"
                                  x-cache-directory)
      backup-directory-alist `(("." . ,(expand-file-name
                                        "backups"
                                        x-cache-directory))))

(when (featurep 'native-compile)
  (startup-redirect-eln-cache
   (expand-file-name (expand-file-name "eln/" x-cache-directory))))

;; Don’t call the garbage collector during initialization
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold (* 512 1-MiB))))

(setq read-process-output-max
      (let ((pipe-size-file "/proc/sys/fs/pipe-max-size"))
	(if (file-exists-p pipe-size-file)
            (with-temp-buffer
              (insert-file-contents pipe-size-file)
              (number-at-point))
          1-MiB)))

(setq package-enable-at-startup nil)
