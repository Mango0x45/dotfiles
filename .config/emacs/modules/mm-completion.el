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


;;; Minibuffer Completion Styles

(use-package minibuffer
  :bind ( :map minibuffer-local-completion-map
          ("SPC" . nil)
          ("?"   . nil))
  :custom
  (completion-styles '(basic substring orderless))
  (completion-category-defaults nil)    ; Avoid needing to override things
  (completion-category-overrides
   '((file             (styles . (basic partial-completion orderless)))
     (bookmark         (styles . (basic substring)))
     (library          (styles . (basic substring)))
     (imenu            (styles . (basic substring orderless)))
     (consult-location (styles . (basic substring orderless)))
     (kill-ring        (styles . (basic substring orderless)))))
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t))

(use-package orderless
  :ensure t
  :after minibuffer
  :custom
  (orderless-matching-styles '(orderless-prefixes orderless-regexp)))


;;; Disable Minibuffer Recursion Level

(use-package mb-depth
  :hook (after-init . minibuffer-depth-indicate-mode)
  :custom
  (enable-recursive-minibuffers t))


;;; Don’t Show Defaults After Typing

;; Usually if a minibuffer prompt has a default value you can access by
;; hitting RET, the prompt will remain even if you begin typing (meaning
;; the default will no longer take effect on RET).  Enabling this mode
;; disables that behaviour.

(use-package minibuf-eldef
  :hook (after-init . minibuffer-electric-default-mode)
  :custom
  (minibuffer-default-prompt-format " [%s]"))


;;; Hide Shadowed Filepaths

(use-package rfn-eshadow
  :hook (after-init . file-name-shadow-mode)
  :custom
  (file-name-shadow-properties '(invisible t intangilble t)))


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
  (corfu-auto-delay .1)
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