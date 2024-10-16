;;; mm-theme.el --- Emacs theme settings  -*- lexical-binding: t; -*-


;;; Custom Theme

(load-theme 'mango :no-confirm)


;;; Fonts

(defvar mm-theme-monospace-font `(,(if mm-darwin-p
                                       "Iosevka Custom"
                                     "Iosevka Smooth")
                                  :weight regular
                                  :height 162)
  "The default monospace font.
This is a plist containing a font name, -weight, and -height.")

(defvar mm-theme-proportional-font '("SF Pro" :weight regular :height 162)
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
      (dolist (rune '(?… ?— ?← ?→ ?⇐ ?⇒ ?⇔))
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

;; Ligature Settings

(defvar mm-theme-ligatures-alist
  `(((c-mode c-ts-mode c++-mode c++-ts-mode)
     . ("->"))
    ((c++-mode c++-ts-mode)
     . ("::"))
    ((js-mode js-ts-mode typescript-ts-mode vue-ts-mode)
     . (("=" ,(rx (or ?> (** 1 2 ?=))))
        ("!" ,(rx        (** 1 2 ?=)))))
    (go-ts-mode
     . (":=" "<-"))
    ((python-mode python-ts-mode)
     . (":=" "->"))
    ((mhtml-mode html-mode html-ts-mode vue-ts-mode)
     . ("<!--" "-->" "/>"))
    (prog-mode
     . ("<=" ">=" "==" "!=" "*=" "__")))
  "Ligatures to enable in specific modes.
Elements of this alist are of the form:

  (SPEC . LIGATURES)

Where LIGATURES is a list of ligatures to enable for the set of modes
described by SPEC.

SPEC can be either a symbol, or a list of symbols.  These symbols should
correspond to modes for which the associated LIGATURES should be enabled.

A mode may also be specified in multiple entries.  To configure
`go-ts-mode' to have its set of ligatures be a super-set of the
ligatures for `c-ts-mode', the following two entries could be added:

  \\='((c-ts-mode go-ts-mode) . (\">=\" \"<=\" \"!=\" \"==\"))
  \\='(go-ts-mode             . (\":=\"))")

(defun mm-theme-update-ligatures ()
  "Update the ligature composition tables.
After running this function you may need to restart `ligature-mode'.

Also see `mm-theme-ligatures-alist'."
  (interactive)
  (setopt ligature-composition-table nil)
  (cl-loop for (spec . ligatures) in mm-theme-ligatures-alist
           do (ligature-set-ligatures spec ligatures)))

(use-package ligature
  :ensure t
  :if (and (or mm-darwin-p
               (seq-contains-p (split-string system-configuration-features)
                               "HARFBUZZ"))
           (display-graphic-p))
  :commands ligature-mode
  :init
  ;; Add ‘ligature-mode’ as a hook for all modes configured in
  ;; ‘mm-theme-ligatures-alist’
  (thread-last
    mm-theme-ligatures-alist
    (mapcar #'car)
    (flatten-tree)
    (seq-uniq)
    (mapcar #'mm-mode-to-hook)
    (mapc (lambda (mode) (add-hook mode #'ligature-mode))))
  :config
  (mm-theme-update-ligatures))


;;; Background Opacity

(defvar mm-theme-background-opacity 100
  "Opacity of the graphical Emacs frame.
A value of 0 is fully transparent while 100 is fully opaque.")

(defun mm-theme-background-opacity (opacity)
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
  :init
  (window-divider-mode))


;;; Pulse Line on Jump

(use-package pulsar
  :ensure t
  :demand t
  :custom
  (pulsar-pulse t)
  (pulsar-delay .05)
  (pulsar-iterations 10)
  :config
  (add-to-list 'pulsar-pulse-functions #'jump-to-register)
  (add-to-list 'pulsar-pulse-functions #'e/scroll-up)
  (add-to-list 'pulsar-pulse-functions #'e/scroll-down)
  ;; Integrate with ‘compilation-mode’
  (add-hook 'next-error-hook #'pulsar-pulse-line)
  (pulsar-global-mode)
  :hook
  ((next-error . (pulsar-pulse-line-red
                  pulsar-recenter-top
                  pulsar-reveal-entry))
   (minibuffer-setup . pulsar-pulse-line-red))
  :bind
  (("C-c h l" . pulsar-highlight-dwim)))


;;; Add Padding

(use-package spacious-padding
  :ensure t
  :demand t
  :config
  (spacious-padding-mode))


;;; Pretty Page Boundaries

(use-package page-break-lines
  :ensure t
  :demand t
  :init
  (add-hook
   'change-major-mode-hook
   (defun mm-theme--set-page-break-max-width ()
     (setopt page-break-lines-max-width fill-column)))
  (global-page-break-lines-mode)
  :config
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


;;; More Intuiative UI for Certain Modes

(use-package line-selection-mode
  :hook ((dired-mode   . line-selection-mode)
         (ibuffer-mode . line-selection-mode)))

(provide 'mm-theme)