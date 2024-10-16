;;; mm-completion.el --- Configuration for Emacs completion  -*- lexical-binding: t; -*-


;;; Vertical Completions

(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode)
  :config
  ;; Highlight the current line
  (require 'hl-line))


;;; Annotate Completions

(use-package marginalia
  :ensure t
  :custom
  (marginalia-field-width 50)
  :init
  (marginalia-mode))


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
  :hook ((prog-mode . corfu-mode))
  :bind (:map corfu-map
         ("C-<return>" . newline))
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0))


;;; Save Minibuffer History

(use-package savehist-mode
  :init
  (savehist-mode))

(provide 'mm-completion)
