;;; mm-search.el --- Emacs text searching  -*- lexical-binding: t; -*-

;;; Classic Emacs text search

(use-package isearch
  :demand t
  :custom
  (search-whitespace-regexp ".*?")
  (isearch-lax-whitespace t)
  (isearch-regexp-lax-whitespace nil)
  (isearch-lazy-count t)
  (lazy-highlight-initial-delay 0)
  (lazy-count-prefix-format "%d/%d ")
  (isearch-repeat-on-direction-change t))


;;; Grab Integration

;; PKG-INTERN
(use-package grab
  :commands ( grab git-grab project-grab project-git-grab
              dired-grab-marked-files)
  :custom
  (grab-default-pattern '("x/^.*?$/ g// h//" . 12)))

(provide 'mm-search)
