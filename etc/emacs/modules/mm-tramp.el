;;; mm-tramp.el --- Tramp configuration  -*- lexical-binding: t; -*-

(use-package tramp
  :custom
  (tramp-auto-save-directory
   (expand-file-name "tramp/auto-save/" mm-cache-directory))
  (tramp-persistency-file-name
   (expand-file-name "tramp/persistency.el" mm-cache-directory))
  (tramp-default-method "ssh")
  (tramp-use-ssh-controlmaster-options nil)
  (remote-file-name-inhibit-cache nil))

(use-package vc
  :after tramp
  :config
  ;; Disable built-in ‘vc-mode’ for remote files
  (setq vc-ignore-dir-regexp
        (format "%s\\|%s" vc-ignore-dir-regexp tramp-file-name-regexp)))

(defun mm-tramp-magit-tramp-settings ()
  "Disable performance-heavy magit sections only for remote repositories."
  (when (file-remote-p default-directory)
    (remove-hook 'magit-status-sections-hook
                 'magit-insert-tags-header
                 :local)
    (remove-hook 'magit-status-sections-hook
                 'magit-insert-unpushed-to-pushremote
                 :local)
    (remove-hook 'magit-status-sections-hook
                 'magit-insert-unpulled-from-pushremote
                 :local)
    (remove-hook 'magit-status-sections-hook
                 'magit-insert-unpulled-from-upstream
                 :local)
    (remove-hook 'magit-status-sections-hook
                 'magit-insert-unpushed-to-upstream-or-recent
                 :local)))

(use-package magit
  :after tramp
  :hook (magit-status-mode . mm-tramp-magit-tramp-settings))

(provide 'mm-tramp)
