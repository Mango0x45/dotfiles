;;; mm-spellcheck.el --- Spell checking configuration  -*- lexical-binding: t; -*-

;;; ISpell Spell Checking

(use-package ispell
  :hook (flyspell-mode . ispell-minor-mode)
  :custom
  (ispell-program-name "hunspell")
  (ispell-local-dictionary "en_US")
  (ispell-local-dictionary-alist
   '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[’']"
      nil ("-d" "en_US") nil utf-8)))
  (ispell-hunspell-dictionary-alist ispell-local-dictionary-alist))


;;; On-The-Fly Spell Checking

(use-package flyspell
  :defer t
  :config
  ;; I use ‘C-,’ for ‘emmet-expand-line’
  (keymap-unset flyspell-mode-map "C-," :remove))

(provide 'mm-spellcheck)
