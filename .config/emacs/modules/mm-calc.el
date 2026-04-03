;;; mm-calc.el --- Emacs configurations for ‘calc-mode’  -*- lexical-binding: t; -*-

(use-package calc
  :init
  (setopt
   calc-display-trail nil
   calc-group-digits t
   ;; Optimize for Europeans
   calc-point-char ","
   calc-group-char "."))

(provide 'mm-calc)
