;;; mm-auth.el --- Authentication in Emacs  -*- lexical-binding: t; -*-

(use-package auth-source
  :custom
  ;; I want Emacs to prompt me for my password instead of storing it
  ;; in my home directory in plaintext.  I can have it GPG encrypted,
  ;; but I don’t use GPG.
  (auth-sources nil))

(provide 'mm-auth)
