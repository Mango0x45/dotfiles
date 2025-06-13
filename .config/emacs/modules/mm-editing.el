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


;;; Recenter The Screen When Jumping

(mm-comment
  (dolist (command #'(backward-page
                      backward-paragraph
                      forward-page
                      forward-paragraph
                      pop-global-mark))
    (advice-add command :around #'mm-do-and-center)))


;;; Indentation Settings

(setq-default
 tab-width 4
 indent-tabs-mode (not mm-humanwave-p))

(defvar mm-editing-indentation-settings-alist
  '((awk-ts-mode           . (:extras awk-ts-mode-indent-level))
    (c-mode                . (:extras c-basic-offset))
    (c-ts-mode             . (:extras c-ts-mode-indent-offset))
    (css-mode              . (:extras css-indent-offset))
    (emacs-lisp-mode       . (:width 8 :spaces t)) ; GNU code uses 8-column tabs
    (go-mod-ts-mode        . (:extras go-ts-mode-indent-offset))
    (go-ts-mode            . (:extras go-ts-mode-indent-offset))
    (gsp-ts-mode           . (:width 2 :extras gsp-ts-mode-indent-rules))
    (helpful-mode          . (:width 8)) ; GNU code uses 8-column tabs
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
those should be listed in `mm-editing-indentation-settings'."
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


;;; Code Commenting

(defun mm-c-comment-no-continue ()
  (setq-local comment-continue "   "))

(defun mm-mhtml-comment-no-continue ()
  (setq-local comment-continue "     "))

(use-package newcomment
  :custom
  (comment-style 'multi-line)
  :config
  (dolist (mode '(c-mode c++-mode))
    (add-hook (mm-mode-to-hook mode) #'mm-c-comment-no-continue)
    (when-let ((ts-mode (mm-mode-to-ts-mode mode))
               ((fboundp ts-mode)))
      (add-hook (mm-mode-to-hook ts-mode) #'mm-c-comment-no-continue)))
  (add-hook 'mhtml-mode #'mm-mhtml-comment-no-continue))


;;; Multiple Cursors

(defmacro mm--define-mc-marking-command (name search-function noun)
  (let ((noun-symbol (intern noun)))
    `(defun ,name (beg end ,noun-symbol)
       ,(format "Mark all occurances of %s between BEG and END.
If called interactively with an active region then all matches in the
region are marked, otherwise all matches in the buffer are marked."
                (upcase noun))
       (interactive
        (list (or (use-region-beginning) (point-min))
              (or (use-region-end) (point-max))
              (read-string
               (format-prompt ,(concat "Match " noun) nil))))
       (if (string-empty-p ,noun-symbol)
           (message "Command aborted")
         (catch 'mm--no-match
           (mc/remove-fake-cursors)
           (goto-char beg)
           (let (did-match-p)
             (while (,search-function ,noun-symbol end :noerror)
               (setq did-match-p t)
               (push-mark (match-beginning 0))
               (exchange-point-and-mark)
               (mc/create-fake-cursor-at-point)
               (goto-char (mark)))
             (unless did-match-p
               (message "No match for `%s'" ,noun-symbol)
               (throw 'mm--no-match nil)))
           (when-let ((first (mc/furthest-cursor-before-point)))
             (mc/pop-state-from-overlay first))
           (if (> (mc/num-cursors) 1)
               (multiple-cursors-mode 1)
             (multiple-cursors-mode 0)))))))

(mm--define-mc-marking-command
 mm-mark-all-in-region search-forward "string")
(mm--define-mc-marking-command
 mm-mark-all-in-region-regexp re-search-forward "regexp")

(mm-comment
  (defun mm-silent-mc-load (args)
    "Advice to `load' to force it to be silent.
The `multiple-cursors' package loads an `.mc-lists.el' file without
passing `:nomessage' which causes messages to be sent in the minibuffer
and *Messages* buffer.  This forces that to not happen."
    (when (and (boundp 'mc/list-file)
               (string= (file-truename (car args))
                        (file-truename mc/list-file)))
      (pcase (length args)
        (1 (setq args (list (car args) nil :nomessage)))
        (2 (add-to-list 'args :nomessage :append))
        (_ (setf (caddr args) :nomessage)))
      (advice-remove #'load #'mm-silent-mc-load))
    args))

(use-package multiple-cursors
  :ensure t
  :demand t
  :bind (("C->"   . #'mc/mark-next-like-this)
         ("C-<"   . #'mc/mark-previous-like-this)
         ("C-M-<" . #'mc/mark-all-like-this-dwim)
         ("C-M->" . #'mc/edit-lines)
         :map search-map
         ("$"     . #'mm-mark-all-in-region)
         ("M-$"   . #'mm-mark-all-in-region-regexp))
  :commands ( mm-mark-all-in-region mm-mark-all-in-region-regexp
              mm-add-cursor-to-next-thing mm-transpose-cursor-regions)
  :init
  (mm-comment
    (advice-add #'load :filter-args #'mm-silent-mc-load)
    (with-eval-after-load 'multiple-cursors-core
      (dolist (command #'(backward-delete-char
                          capitalize-dwim
                          delete-backward-char
                          delete-forward-char
                          downcase-dwim
                          upcase-dwim))
        (add-to-list 'mc/cmds-to-run-for-all command))
      (dolist (command #'(helpful-callable
                          helpful-key
                          helpful-symbol
                          helpful-variable))
        (add-to-list 'mc/cmds-to-run-once command))))
  (with-eval-after-load 'multiple-cursors-core
    (keymap-unset mc/keymap "<return>" :remove)))


;;; Increment Numbers

(use-package increment
  :bind (("C-c i i" . #'increment-number-at-point)
         ("C-c i d" . #'decrement-number-at-point))
  :commands (increment-number-at-point decrement-number-at-point))


;;; Move Line or Region

(defun mm-move-text-indent (&rest _)
  (let ((deactivate deactivate-mark))
    (if (region-active-p)
        (indent-region (region-beginning) (region-end))
      (indent-region (line-beginning-position) (line-end-position)))
    (setq deactivate-mark deactivate)))

(use-package move-text
  :ensure t
  :bind (("M-n" . move-text-down)
         ("M-p" . move-text-up))
  :config
  (dolist (command #'(move-text-up move-text-down))
    (advice-add command :after #'mm-move-text-indent)))


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


;;; Emmet Mode

(defun mm-editing-emmet-dwim (arg)
  "Do-What-I-Mean Emmet expansion.
If the region is active then the region will be surrounded by an emmet
expansion read from the minibuffer.  Otherwise the emmet expression
before point is expanded.  When provided a prefix argument the behaviour
is as described by `emmet-expand-line'."
  (interactive "P")
  (if (region-active-p)
      (call-interactively #'emmet-wrap-with-markup)
    (emmet-expand-line arg)))

(use-package emmet-mode
  :ensure t
  :bind ("C-," . mm-editing-emmet-dwim)
  :custom
  (emmet-self-closing-tag-style ""))


;;; Number Formatting

(use-package number-format-mode
  :commands ( number-format-buffer number-format-region
              number-unformat-buffer number-unformat-region
              number-format-mode))


;;; Additional Major Modes

(use-package awk-ts-mode :ensure t)
(use-package git-modes   :ensure t)
(use-package po-mode     :ensure t)
(use-package sed-mode    :ensure t)

(use-package csv-mode
  :ensure t
  :custom
  (csv-align-style 'auto)
  (csv-align-padding 2))

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
  (add-to-list 'auto-mode-alist (cons pattern 'nroff-mode)))

(provide 'mm-editing)