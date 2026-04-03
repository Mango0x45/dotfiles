;;; mm-window.el --- Window configurations  -*- lexical-binding: t; -*-


;;; Unique Buffer Names

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'forward))


;;; Highlight Whitespace

(use-package whitespace
  :bind (("<f1>"  . whitespace-mode)
         ("C-c z" . delete-trailing-whitespace))
  :custom
  (whitespace-style
   '( face trailing spaces tabs space-mark tab-mark empty indentation
      space-after-tab space-before-tab))
  (whitespace-display-mappings
   '((space-mark  32 [?·] [?.])         ; Space
     (space-mark 160 [?␣] [?_])         ; Non-Breaking Space
     (tab-mark 9 [?» ?\t] [?> ?\t]))))


;;; Line Numbers

(use-package display-line-numbers
  :bind ("<f2>" . display-line-numbers-mode)
  :custom
  (display-line-numbers-grow-only t)
  (display-line-numbers-type 'relative)
  (display-line-numbers-width-start 99))


;;; Select Help Windows

(use-package help
  :custom
  (help-window-select t))


;;; Window Scrolling

(use-package window
  :custom
  (scroll-conservatively 101)           ; (info "(Emacs)Auto Scrolling")
  (scroll-error-top-bottom t)
  (scroll-margin 10)
  :config
  (setq-default truncate-partial-width-windows nil))


;;; Smoother Scrolling

(mm-comment
  (use-package pixel-scroll
    :init
    (pixel-scroll-precision-mode)
    :config
    ;; Make it easier to use custom scroll functions
    (dolist (binding '("<next>" "<prior>"))
      (keymap-unset pixel-scroll-precision-mode-map binding :remove))))


;;; Ace Window

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))

(provide 'mm-window)