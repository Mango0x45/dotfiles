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
  (window-resize-pixelwise t)
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

;; PKG-EXTERN
(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window)
  :custom
  (aw-make-frame-char ?.)
  (aw-scope 'frame)
  ;; Use uppercase labels because they look nicer, but allow selecting
  ;; with lowercase so that I don’t need to hold shift.
  (aw-keys (cl-loop for x from ?A to ?Z collect x))
  (aw-translate-char-function #'upcase))


;;; Tab Bar Configuration

(use-package tab-bar
  :custom
  (tab-bar-separator "")
  (tab-bar-new-button-show nil)
  (tab-bar-close-button-show nil))


;;; Set Fringes

(use-package fringe
  :init
  (add-hook 'after-init-hook (defun mm-window-set-fringes ()
                               (fringe-mode 16))))

(provide 'mm-window)
