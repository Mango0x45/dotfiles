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

(provide 'mm-search)