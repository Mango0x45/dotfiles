;;; mm-editing.el --- Text editing configuation  -*- lexical-binding: t; -*-

;;; Delete Region When Typing

(use-package delsel
  :hook (after-init . delete-selection-mode))


;;; Capitalize ‘ß’ into ‘ẞ’

;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2024-11/msg00030.html
(set-case-syntax-pair ?ẞ ?ß (standard-case-table))
(put-char-code-property ?ß 'special-uppercase nil)


;;; Force Spaces For Alignment

(defun mm-editing-force-space-indentation (function &rest arguments)
  "Call FUNCTION with ARGUMENTS in an environment in which
`indent-tabs-mode' is nil."
  (let (indent-tabs-mode)
    (apply function arguments)))

(dolist (command #'(align-region
                    c-backslash-region
                    comment-dwim
                    makefile-backslash-region
                    sh-backslash-region))
  (advice-add command :around #'mm-editing-force-space-indentation))


;;; Indentation Settings

(setq-default
 tab-width 4
 indent-tabs-mode (not mm-humanwave-p))

(defvar mm-editing-indentation-settings-alist
  '((awk-ts-mode           . (:extras awk-ts-mode-indent-level))
    (c-mode                . (:extras c-basic-offset))
    (c-ts-mode             . (:extras c-ts-mode-indent-offset))
    (css-mode              . (:extras css-indent-offset))
    (elixir-ts-mode        . (:width 2 :extras elixir-ts-indent-offset))
    (emacs-lisp-mode       . (:width 8 :spaces t)) ; GNU code uses 8-column tabs
    (go-mod-ts-mode        . (:extras go-ts-mode-indent-offset))
    (go-ts-mode            . (:extras go-ts-mode-indent-offset))
    (gsp-ts-mode           . (:width 2 :extras gsp-ts-mode-indent-rules))
    (helpful-mode          . (:width 8)) ; GNU code uses 8-column tabs
    (json-ts-mode          . (:extras json-ts-mode-indent-offset))
    (latex-mode            . (:width 2))
    (lisp-data-mode        . (:spaces t))
    (lisp-interaction-mode . (:spaces t))
    (lisp-mode             . (:spaces t))
    (mhtml-mode            . (:extras sgml-basic-offset))
    (org-mode              . (:width 8 :spaces t))
    (python-mode           . (:extras python-indent-offset))
    (python-ts-mode        . (:extras python-indent-offset))
    (sgml-mode             . (:extras sgml-basic-offset))
    (sh-mode               . (:extras sh-basic-offset))
    (sql-mode              . (:extras sqlind-basic-offset))
    (tex-mode              . (:width 2))
    (typescript-ts-mode    . (:extras typescript-ts-mode-indent-offset))
    (vimscript-ts-mode     . (:extras vimscript-ts-mode-indent-level))
    (vue-ts-mode           . (:extras (typescript-ts-mode-indent-offset
                                       vue-ts-mode-indent-offset))))
  "Alist of indentation settings.
Each pair in this alist is of the form (MODE . SETTINGS) where MODE
specifies the mode for which the given SETTINGS should apply.

SETTINGS is a plist of one-or-more of the following keys:

  `:spaces' -- If nil force tabs for indentation, if non-nil for spaces
               for indentation.  If this key is not provided then the
               value of `indent-tabs-mode' is used.
  `:width'  -- Specifies a non-negative number to be used as the tab
               width and indentation offset.  If this key is not
               provided then the default value of `tab-width' is used.
  `:extras' -- A list of mode-specific variables which control
               indentation settings that need to be set for
               configurations to properly be applied.")

(defun mm-editing-set-indentation-settings ()
  "Set indentation settings for the current major mode.
The indentation settings are set based on the configured values in
`mm-editing-indentation-settings-alist'."
  (let* ((plist (alist-get major-mode mm-editing-indentation-settings-alist))
         (spaces (plist-member plist :spaces))
         (width  (plist-member plist :width))
         (extras (plist-member plist :extras)))
    ;; Some modes like ‘python-mode’ explicitly set ‘tab-width’ and
    ;; ‘indent-tabs-mode’ so we must override them explicitly.
    (setq-local indent-tabs-mode (if spaces (not (cadr spaces))
                                   (default-value 'indent-tabs-mode))
                tab-width (or (cadr width) (default-value 'tab-width)))
    (when extras
      (setq extras (cadr extras))
      (when (symbolp extras)
        (setq extras (list extras)))
      (dolist (extra extras)
        (set extra tab-width)))))

(add-hook 'after-change-major-mode-hook #'mm-editing-set-indentation-settings)

(defun mm-editing-set-tabsize ()
  "Set the tabsize for the current buffer.
If the current buffer’s major mode requires setting additional variables,
those should be listed in `mm-editing-indentation-settings-alist'."
  (declare (interactive-only t))
  (interactive)
  (let* ((prompt-default (default-value 'tab-width))
         (prompt (format-prompt "Tabsize" prompt-default))
         (tabsize (mm-as-number (read-string prompt nil nil prompt-default))))
    (setq-local tab-width tabsize)
    (when-let* ((plist (alist-get major-mode mm-editing-indentation-settings))
                (extras (plist-get plist :extras)))
      (dolist (extra (if (symbolp extras)
                         (list extras)
                       extras))
        (set (make-local-variable extra) tabsize)))))

(use-package sh-mode
  :custom
  (sh-indent-for-case-label 0)
  (sh-indent-for-case-alt #'+))

;; Indent with tabs and align with spaces
;; PKG-INTERN
(use-package smart-tabs-mode
  :load-path (lambda () (expand-file-name "site-lisp/smart-tabs-mode.el"
                                          mm-config-directory))
  :hook after-init)


;;; Code Commenting

(defvar mm-editing-comment-settings-alist
  '(((c-mode c++-mode) . ("/* " "   " " */"))
    ;; rustfmt doesn’t play nice, so we need the ‘*’ comment
    ;; continuation
    (rust-mode . ("/* " " * " " */")))
  "TODO")

(defun mm-newcomment-rust-config ()
  (setq-local comment-quote-nested nil))

(use-package newcomment
  :custom
  (comment-style 'multi-line)
  :config
  (dolist (record mm-editing-comment-settings-alist)
    (let* ((modes (car record))
           (modes (if (listp modes) modes (list modes)))
           (config (cdr record))
           (set-comment-settings
            (lambda ()
              (setq-local comment-start    (nth 0 config)
                          comment-continue (nth 1 config)
                          comment-end      (nth 2 config)))))
        (dolist (mode modes)
          (let ((ts-mode (mm-mode-to-ts-mode mode)))
            (when (fboundp mode)
              (add-hook (mm-mode-to-hook mode) set-comment-settings))
            (when (fboundp ts-mode)
              (add-hook (mm-mode-to-hook ts-mode) set-comment-settings)))))))


;;; Multiple Cursors

;; PKG-INTERN
(use-package multiple-cursors-extensions
  :after multiple-cursors
  :bind (("C-M-@" . #'mce-add-cursor-to-next-word)
         ("C-M-o" . #'mce-add-cursor-to-next-symbol)
         :map search-map
         ("$"     . #'mce-mark-all-in-region)
         ("M-$"   . #'mce-mark-all-in-region-regexp))
  :commands (mce-add-cursor-to-next-symbol
             mce-add-cursor-to-next-word
             mce-mark-all-in-region
             mce-mark-all-in-region-regexp
             mce-sort-regions
             mce-transpose-cursor-regions))

;; PKG-EXTERN
(use-package multiple-cursors
  :ensure t
  :demand t
  :bind (("C->"   . #'mc/mark-next-like-this)
         ("C-<"   . #'mc/mark-previous-like-this)
         ("C-M-<" . #'mc/mark-all-like-this-dwim)
         ("C-M->" . #'mc/edit-lines))
  :commands ( mm-editing-mark-all-in-region mm-editing-mark-all-in-region-regexp
              mm-add-cursor-to-next-thing mm-transpose-cursor-regions)
  :init
  (with-eval-after-load 'multiple-cursors-core
    (keymap-unset mc/keymap "<return>" :remove)))


;;; Increment Numbers

;; PKG-INTERN
(use-package increment
  :bind (("C-c a" . #'increment-number-at-point)
         ("C-c x" . #'decrement-number-at-point))
  :commands (increment-number-at-point decrement-number-at-point))


;;; Move Line or Region

(defun mm-editing-move-text-indent (&rest _)
  (let ((deactivate deactivate-mark))
    (if (region-active-p)
        (indent-region (region-beginning) (region-end))
      (indent-region (line-beginning-position) (line-end-position)))
    (setq deactivate-mark deactivate)))

;; PKG-EXTERN
(use-package move-text
  :ensure t
  :bind (("M-n" . move-text-down)
         ("M-p" . move-text-up))
  :config
  (dolist (command #'(move-text-up move-text-down))
    (advice-add command :after #'mm-editing-move-text-indent)))


;;; Surround With Delimeters

(defun mm-editing-surround-with-spaces (char)
  "Surrounds region or current symbol with a pair defined by CHAR.
This is the same as `surround-insert' except it pads the contents of the
surround with spaces."
  (interactive
   (list (char-to-string (read-char "Character: "))))
  (let* ((pair (surround--make-pair char))
         (left  (car pair))
         (right (cdr pair))
         (bounds (surround--infer-bounds t)))
    (save-excursion
      (goto-char (cdr bounds))
      (insert " " right)
      (goto-char (car bounds))
      (insert left " "))
    (when (eq (car bounds) (point))
      (forward-char))))

;; TODO: Implement this manually
;; PKG-EXTERN
(use-package surround
  :ensure t
  :bind-keymap ("M-'" . surround-keymap)
  :bind (:map surround-keymap
         ("S" . #'mm-editing-surround-with-spaces))
  :config
  (dolist (pair '(("‘" . "’")
                  ("“" . "”")
                  ("»" . "«")
                  ("⟮" . "⟯")))
    (push pair surround-pairs))
  (make-variable-buffer-local 'surround-pairs)
  (add-hook 'emacs-lisp-mode-hook
            (defun mm-editing-add-elisp-quotes-pair ()
              (push '("`" . "'") surround-pairs))))


;;; Insert Webpage Contents

(defun mm-editing-insert-from-url (url)
  "Insert the contents of URL at point."
  (interactive
   (progn
     (barf-if-buffer-read-only)
     (let ((url-at-point (thing-at-point 'url)))
       (list (read-string
              (format-prompt "URL" url-at-point)
              nil nil url-at-point)))))
  (call-process "curl" nil '(t nil) nil url))


;;; Emmet Mode

(defun mm-editing-emmet-dwim (arg)
  "Do-What-I-Mean Emmet expansion.
If the region is active then the region will be surrounded by an emmet
expansion read from the minibuffer.  Otherwise the emmet expression
before point is expanded.  When provided a prefix argument the behaviour
is as described by `emmet-expand-line'."
  (interactive "*P")
  (if (region-active-p)
      (call-interactively #'emmet-wrap-with-markup)
    (emmet-expand-line arg)))

;; PKG-EXTERN
(use-package emmet-mode
  :ensure t
  :bind ("C-," . mm-editing-emmet-dwim)
  :custom
  (emmet-self-closing-tag-style ""))

(defun mm-editing-set-closing-tag-style ()
  (setq-local emmet-self-closing-tag-style " /"))

(use-package emmet-mode
  :hook (vue-ts-mode . mm-editing-set-closing-tag-style)
  :after vue-ts-mode)


;;; JQ Manipulation in JSON Mode

;; PKG-INTERN
(use-package jq
  :commands (jq-filter-region jq-live))


;;; Number Formatting

;; PKG-INTERN
(use-package number-format-mode
  :commands ( number-format-buffer number-format-region
              number-unformat-buffer number-unformat-region
              number-format-mode))


;;; Additional Major Modes

(use-package awk-ts-mode :ensure t)     ; PKG-EXTERN
(use-package cmake-mode  :ensure t)     ; PKG-EXTERN
(use-package git-modes   :ensure t)     ; PKG-EXTERN
(use-package kdl-mode    :ensure t)     ; PKG-EXTERN
(use-package po-mode     :ensure t)     ; PKG-EXTERN
(use-package sed-mode    :ensure t)     ; PKG-EXTERN

;; PKG-EXTERN
(use-package csv-mode
  :ensure t
  :custom
  (csv-align-style 'auto)
  (csv-align-padding 2))

(use-package csv-mode
  :hook (csv-mode . number-format-mode)
  :after number-format-mode)

;; PKG-EXTERN
(use-package git-commit-ts-mode
  :ensure t
  :hook (git-commit-ts-mode . auto-fill-mode)
  :custom
  (git-commit-ts-max-message-size 50))

;; PKG-INTERN
(use-package xcompose-mode
  :vc ( :url "https://git.thomasvoss.com/xcompose-mode"
        :branch "master"
        :rev :newest
        :vc-backend Git)
  :ensure t)


;;; Mode-Specific Configurations

(use-package make-mode
  :custom
  (makefile-backslash-column 80))

(use-package python-mode
  :custom
  (python-indent-def-block-scale 1)
  (python-indent-guess-indent-offset-verbose nil))


;;; Add Missing Extensions

(dolist (pattern '("\\.tmac\\'" "\\.mom\\'"))
  (add-to-list 'auto-mode-alist (cons pattern #'nroff-mode)))


;;; Subword Navigation

(use-package subword
  :hook prog-mode)


;;; Make Camel-Case More Readable

(defvar mm-editing-camel-case-modes
  '(js-mode js-ts-mode java-mode java-ts-mode))

(use-package glasses-mode
  :init
  (dolist (mode mm-editing-camel-case-modes)
    (add-hook (mm-mode-to-hook mode) #'glasses-mode))
  :custom
  (glasses-separate-parentheses-p nil))


;;; Doasedit support

;; PKG-INTERN
(use-package doasedit
  :hook (after-init . global-doasedit-mode))

(provide 'mm-editing)
