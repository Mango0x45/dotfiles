;;; mm-completion.el --- Configuration for Emacs completion  -*- lexical-binding: t; -*-


;;; Vertical Completions

(use-package vertico
  :ensure t
  :hook after-init
  :custom
  (vertico-cycle t)
  :config
  (require 'hl-line))


;;; Annotate Completions

;; TODO: Show git branch descriptions!
(use-package marginalia
  :ensure t
  :hook after-init
  :custom
  (marginalia-field-width 50))


;;; Orderless Completion Style

;; TODO: Make sure this doesn’t suck
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (orderless-matching-styles '(orderless-prefixes))
  (completion-category-overrides '((file (styles basic partial-completion)))))


;;; Completion Popups

(use-package corfu
  :ensure t
  :hook prog-mode
  :bind ( :map corfu-map
          ("C-<return>" . newline))
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-auto-prefix 1)
  (corfu-auto-delay .2)
  :config
  ;; I complete with RET and this interferes with ‘tempel-next’
  (keymap-unset corfu-map "TAB" :remove)
  (with-eval-after-load 'savehist
    (corfu-history-mode)
    (add-to-list 'savehist-additional-variables 'corfu-history)))


;;; Save Minibuffer History

(use-package savehist-mode
  :hook (after-init . savehist-mode))


;;; Enhanced Replacements for Builtins

;; TODO: Investigate other commands
(use-package consult
  :ensure t
  :bind ( ([remap switch-to-buffer] . consult-buffer)
          ([remap imenu]            . consult-imenu)
          ([remap goto-line]        . consult-goto-line)
          :map consult-narrow-map
          ("?" . consult-narrow-help))
  :config
  (with-eval-after-load 'project
    (keymap-set project-prefix-map "b" #'consult-project-buffer))
  (with-eval-after-load 'pulsar
    (setopt consult-after-jump-hook nil)
    (dolist (command #'(pulsar-recenter-top pulsar-reveal-entry))
      (add-hook 'consult-after-jump-hook command))))

(provide 'mm-completion)
