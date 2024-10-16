;;; mm-editing.el --- Text editing configuation  -*- lexical-binding: t; -*-

;;; Delete Region When Typing

(use-package delsel
  :init
  (delete-selection-mode))


;;; Force Spaces For Alignment

(defun mm-editing-force-space-indentation (function &rest arguments)
  "Call FUNCTION with ARGUMENTS in an environment in which
`indent-tabs-mode' is nil."
  (let (indent-tabs-mode)
    (apply function arguments)))

(dolist (command #'(align c-backslash-region comment-dwim))
  (advice-add command :around #'mm-editing-force-space-indentation))


;;; Indentation Settings

(setq-default
 tab-width 4
 indent-tabs-mode t)

(defvar mm-editing-indentation-settings-alist
  '((c-mode            . (:extras c-basic-offset))
    (c-ts-mode         . (:extras c-ts-mode-indent-offset))
    (css-mode          . (:extras css-indent-offset))
    (emacs-lisp-mode   . (:width 8 :spaces t)) ; GNU code uses 8-column tabs
    (go-ts-mode        . (:extras go-ts-mode-indent-offset))
    (go-mod-ts-mode    . (:extras go-ts-mode-indent-offset))
    (gsp-ts-mode       . (:width 2 :extras gsp-ts-mode-indent-rules))
    (helpful-mode      . (:width 8))    ; GNU code uses 8-column tabs
    (lisp-data-mode    . (:spaces t))
    (lisp-mode         . (:spaces t))
    (org-mode          . (:spaces t))
    (python-mode       . (:extras python-indent-offset))
    (python-ts-mode    . (:extras python-indent-offset))
    (sgml-mode         . (:extras sgml-basic-offset))
    (sh-mode           . (:extras sh-basic-offset))
    (vimscript-ts-mode . (:extras vimscript-ts-mode-indent-level)))
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
    (when spaces
      (indent-tabs-mode (and (cadr spaces) -1)))
    (when width
      (setq-local tab-width (cadr width)))
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


;;; Multiple Cursors

(use-package multiple-cursors
  :ensure t
  :bind (("C->"   . #'mc/mark-next-like-this)
         ("C-<"   . #'mc/mark-previous-like-this)
         ("C-M-<" . #'mc/mark-all-like-this-dwim)
         ("C-M->" . #'mc/edit-lines))
  :init
  (with-eval-after-load 'multiple-cursors-core
    (dolist (command #'(delete-backward-char
                        delete-forward-char
                        backward-delete-char
                        capitalize-dwim
                        downcase-dwim
                        upcase-dwim))
      (add-to-list 'mc/cmds-to-run-for-all command))
    (dolist (command #'(helpful-callable
                        helpful-key
                        helpful-symbol
                        helpful-variable))
      (add-to-list 'mc/cmds-to-run-once command))
    (add-to-list 'mc/unsupported-minor-modes #'corfu-mode)))


;;; Increment Numbers

(use-package increment
  :bind (("C-c i i" . #'increment-number-at-point)
         ("C-c i d" . #'decrement-number-at-point))
  :commands (increment-number-at-point decrement-number-at-point))


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
  :bind ("C-," . mm-editing-emmet-dwim))

(provide 'mm-editing)
