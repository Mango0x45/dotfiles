;;; mm-dired.el --- Configure the directory editor  -*- lexical-binding: t; -*-

(use-package dired
  :hook ((dired-mode . dired-omit-mode)
         (dired-mode . dired-hide-details-mode))
  :bind ( :map dired-mode-map
          ("f" . find-file))
  :custom
  (dired-auto-revert-buffer #'dired-directory-changed-p)
  (dired-dwim-target t)
  (dired-free-space nil)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-listing-switches
   (combine-and-quote-strings
    '("-AFGhlv" "--group-directories-first" "--time-style=+%d %b %Y %T"))))

(provide 'mm-dired)