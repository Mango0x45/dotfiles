;;; mm-theme.el --- Emacs theme settings  -*- lexical-binding: t; -*-


;;; Themes

(setopt custom-theme-directory (expand-file-name "themes" mm-config-directory))
(load-theme 'mango-dark :no-confirm)


;;; Disable Cursor Blink

(use-package frame
  :config
  (blink-cursor-mode -1))


;;; Fonts

(defvar mm-theme-monospace-font `(,(if mm-humanwave-p
                                       "Iosevka Custom"
                                     "Iosevka Smooth")
                                  :weight regular
                                  :height 162)
  "The default monospace font.
This is a plist containing a font name, -weight, and -height.")

(defvar mm-theme-proportional-font
  ;; TODO: SF font?
  `(,(if mm-darwin-p "Microsoft Sans Serif" "SF Pro Text")
    :weight regular :height 162)
  "The default proportional font.
This is a plist containing a font name, -weight, and -height.")

(defun mm-theme-set-fonts (&optional _frame)
  "Set frame font settings.
Sets the frame font settings according to the fonts specified by
`mm-theme-monospace-font' and `mm-theme-proportional-font'.

This function can be used as a hook in `after-make-frame-functions' and
_FRAME is ignored."
  (interactive)
  (let* ((mono-family (car mm-theme-monospace-font))
         (mono-props  (cdr mm-theme-monospace-font))
         (prop-family (car mm-theme-proportional-font))
         (prop-props  (cdr mm-theme-proportional-font))
         (mono-weight (plist-get mono-props :weight))
         (mono-height (plist-get mono-props :height))
         (prop-weight (plist-get prop-props :weight))
         (prop-height (plist-get prop-props :height)))
    ;; Some characters in this font are larger than usual
    (when (string= mono-family "Iosevka Smooth")
      (dolist (rune '(?․ ?‥ ?… ?— ?← ?→ ?⇐ ?⇒ ?⇔))
        (set-char-table-range char-width-table rune 2)))
    (set-face-attribute 'default nil
                        :font mono-family
                        :weight mono-weight
                        :height mono-height)
    (set-face-attribute 'fixed-pitch nil
                        :font mono-family
                        :weight mono-weight
                        :height mono-height)
    (set-face-attribute 'variable-pitch nil
                        :font prop-family
                        :weight prop-weight
                        :height prop-height)))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'mm-theme-set-fonts)
  (mm-theme-set-fonts))


;;; Ligature Support

(defvar mm-theme-ligatures-alist
  `(((c-mode c++-mode)
     . ("->"))
    ((c++-mode)
     . ("::"))
    ((js-mode typescript-ts-mode vue-ts-mode)
     . (("=" ,(rx (or ?> (** 1 2 ?=))))
        ("!" ,(rx        (** 1 2 ?=)))))
    (go-ts-mode
     . (":=" "<-"))
    ((python-mode)
     . (":=" "->"))
    ((mhtml-mode html-mode vue-ts-mode)
     . ("<!--" "-->" "/>"))
    (prog-mode
     . ("<<=" "<=" ">=" "==" "!=" "*=" ("_" "_+"))))
  "Ligatures to enable in specific modes.
Elements of this alist are of the form:

  (SPEC . LIGATURES)

Where LIGATURES is a list of ligatures to enable for the set of modes
described by SPEC.

SPEC can be either a symbol, or a list of symbols.  These symbols
should correspond to modes for which the associated LIGATURES should
be enabled.

A mode may also be specified in multiple entries.  To configure
`go-ts-mode' to have its set of ligatures be a super-set of the
ligatures for `c-ts-mode', the following two entries could be added:

  \\='((c-ts-mode go-ts-mode) . (\">=\" \"<=\" \"!=\" \"==\"))
  \\='(go-ts-mode             . (\":=\"))

When a language is specified and it’s tree-sitter compatriot is bound,
then LIGATURES are bound for both modes.")

(defun mm-theme-update-ligatures ()
  "Update the ligature composition tables.
After running this function you may need to restart `ligature-mode'.

Also see `mm-theme-ligatures-alist'."
  (interactive)
  (setopt ligature-composition-table nil)
  (cl-loop for (spec . ligatures) in mm-theme-ligatures-alist
           do (ligature-set-ligatures spec ligatures)
           (cl-loop for mode in spec
                    when (fboundp (mm-mode-to-ts-mode mode))
                    do (ligature-set-ligatures mode ligatures))))

;; PKG-EXTERN
(use-package ligature
  :ensure t
  :if (and (display-graphic-p)
           (or (seq-contains-p (split-string system-configuration-features)
                               "HARFBUZZ")
               mm-darwin-p))
  :hook prog-mode
  :config
  (mm-theme-update-ligatures))


;;; Background Opacity

(defvar mm-theme-background-opacity 100
  "Opacity of the graphical Emacs frame.
A value of 0 is fully transparent while 100 is fully opaque.")

(defun mm-theme-set-background-opacity (opacity)
  "Set the current frames' background opacity.
See also the `mm-theme-background-opacity' variable."
  (interactive
   (list (mm-as-number
          (read-string
           (format-prompt "Background opacity"
                          (default-value 'mm-theme-background-opacity))
           nil nil mm-theme-background-opacity))))
  (set-frame-parameter nil 'alpha-background opacity))

(add-to-list
 'default-frame-alist (cons 'alpha-background mm-theme-background-opacity))


;;; Divider Between Windows

(use-package frame
  :hook (after-init . window-divider-mode))


;;; In-Buffer Highlighting

;; PKG-INTERN
(use-package highlighter
  :bind (("C-c h m" . #'highlighter-mark)
         ("C-c h u" . #'highlighter-unmark)
         ("C-c h U" . #'highlighter-unmark-buffer))
  :commands (highlighter-mark highlighter-unmark highlighter-unmark-buffer)
  :init
  (require 'hi-lock))                   ; For extra face definitions


;;; Pretty Page Boundaries

;; TODO: Implement this myself?
(use-package page-break-lines
  :ensure t
  :hook (after-init . global-page-break-lines-mode)
  :config
  (dolist (mode '(c-mode c++-mode gsp-ts-mode))
    (add-to-list 'page-break-lines-modes mode)
    (let ((ts-mode (mm-mode-to-ts-mode mode)))
      (when (fboundp ts-mode)
        (add-to-list 'page-break-lines-modes ts-mode))))
  (add-hook
   'change-major-mode-hook
   (defun mm-theme--set-page-break-max-width ()
     (setopt page-break-lines-max-width fill-column)))
  ;; Since the ‘^L’ character is replaced by a horizontal rule, the
  ;; cursor should appear below the horizontal rule.  When moving
  ;; backwards we need to account for the fact that the cursor is
  ;; actually one character ahead of hte page break and adjust
  ;; accordingly.
  (advice-add
   #'forward-page :after
   (defun mm-theme--forward-char (&rest _)
     (forward-char)))
  (advice-add
   #'backward-page :before
   (defun mm-theme--backward-char (&rest _)
     (backward-char))))


;;; Line Highlighting

(use-package hl-line
  :custom
  (hl-line-sticky-flag nil))


;;; Indent Guides

(when mm-humanwave-p
  (use-package highlight-indent-guides
    :ensure t
    :hook ((jinja2-mode vue-ts-mode mhtml-mode) . highlight-indent-guides-mode)
    :custom
    (highlight-indent-guides-method 'fill)
    (highlight-indent-guides-auto-even-face-perc 30)
    (highlight-indent-guides-auto-odd-face-perc   0)))


;;; Instantly highlight matching parens

(use-package paren
  :custom
  (show-paren-delay 0))

(provide 'mm-theme)
