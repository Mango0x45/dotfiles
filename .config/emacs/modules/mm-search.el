;;; mm-search.el --- Emacs text searching  -*- lexical-binding: t; -*-

;;; Classic Emacs text search

(use-package isearch
  :demand t
  :bind ( :map isearch-mode-map
          ("M-/" . isearch-complete)
          :map minibuffer-local-isearch-map
          ("M-/" . isearch-complete-edit))
  :custom
  (search-whitespace-regexp ".*?")
  (isearch-lax-whitespace t)
  (isearch-regexp-lax-whitespace nil)
  (isearch-lazy-count t)
  (lazy-highlight-initial-delay 0)
  (lazy-count-prefix-format "(%s/%s) ")
  (isearch-repeat-on-direction-change t))

(defun mm--project-find-wrapper (command regexp)
  (let (csf)
    (cond ((string-suffix-p "/i" regexp)
           (setq regexp (string-remove-suffix "/i" regexp)
                 csf t))
          ((string-suffix-p "/I" regexp)
           (setq regexp (string-remove-suffix "/I" regexp)
                 csf nil))
          (:else
           (setq csf case-fold-search)))
    (let ((case-fold-search csf))
      (funcall-interactively command regexp))))

(defun mm-project-find-regexp (regexp)
  "Find all matches for REGEXP in the current project’s roots.
This is a thin wrapper around `project-find-regexp' that supports the
`/i' and `/I' suffixes to enable and disable case-sensitive matching
respectively."
  (interactive (list (project--read-regexp)))
  (mm--project-find-wrapper #'project-find-regexp regexp))

(defun mm-project-or-external-find-regexp (regexp)
  "Find all matches for REGEXP in the project roots or external roots.
This is a thin wrapper around `project-or-external-find-regexp' that
supports the `/i' and `/I' suffixes to enable and disable case-sensitive
matching respectively."
  (interactive (list (project--read-regexp)))
  (mm--project-find-wrapper #'project-or-external-find-regexp regexp))

(provide 'mm-search)
