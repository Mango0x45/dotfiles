;;; mm-dired.el --- Configure the directory editor  -*- lexical-binding: t; -*-

(use-package dired
  :hook ((dired-mode . dired-omit-mode)
         (dired-mode . dired-hide-details-mode))
  :bind (:map dired-mode-map
         ("f" . find-file)))

(provide 'mm-dired)
