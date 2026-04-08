;;; mm-tramp.el --- Tramp configuration  -*- lexical-binding: t; -*-

(use-package tramp
  :config
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))

  (connection-local-set-profiles
   '(:application tramp :protocol "rsync")
   'remote-direct-async-process)
  :custom
  (tramp-verbose 2))

(use-package tramp
  :after magit
  :custom
  (magit-tramp-pipe-stty-settings 'pty))

(use-package tramp-sh
  :custom
  (tramp-copy-size-limit (* 1024 1024)) ; 1 MiB
  (tramp-use-scp-direct-remote-copying t)
  (tramp-default-method (or (executable-find "rsync") "scp")))

(provide 'mm-tramp)
