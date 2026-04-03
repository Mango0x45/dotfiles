;;; mm-dired.el --- Configure the directory editor  -*- lexical-binding: t; -*-

(defun mm-dired-use-current-directory (function &rest args)
  "Run FUNCTION with ARGS in the current dired directory."
  (let ((default-directory (dired-current-directory)))
    (apply function args)))

(use-package dired
  :hook ((dired-mode . dired-omit-mode)
         (dired-mode . dired-hide-details-mode))
  :bind ( :map dired-mode-map
          ("C-c C-w" . wdired-change-to-wdired-mode)
          ("f"       . dired-x-find-file))
  :config
  (advice-add #'dired-x-read-filename-at-point
              :around #'mm-dired-use-current-directory)
  :custom
  (dired-auto-revert-buffer #'dired-directory-changed-p)
  (dired-dwim-target t)
  (dired-free-space nil)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-hide-details-preserved-columns '(1))
  (dired-listing-switches
   (combine-and-quote-strings
    '("-AFGhlv" "--group-directories-first" "--time-style=+%d %b %Y %T"))))

(use-package dired-aux
  :custom
  (dired-create-destination-dirs 'ask)
  (dired-create-destination-dirs-on-trailing-dirsep t)
  (dired-isearch-filenames 'dwim))

(provide 'mm-dired)
