;;; mango-theme.el --- Just your average dark theme  -*- lexical-binding: t; -*-

(deftheme mango
  "Mildly dark, dark theme.
Your average not-so-dark dark theme, because none of the other options
were exactly to my liking.  It’s about time I had a theme to call my
own.")

(defsubst mango-theme--color (name)
  "Get the RGB value of the COLOR."
  (alist-get name mango-theme-colors-alist))

(defmacro mango-theme--generate-set-faces (&rest body)
  "A macro to provide a much simpler syntax than what is expected by
`custom-theme-set-faces'.  This is possible because I only run Emacs
graphically, so I shouldn’t need to have multiple specs per face.

\(fn SPEC...)"
  (declare (indent 0))
  (let ((ret '('mango custom-theme-set-faces)))
    (dolist (spec body)
      (add-to-list 'ret `(backquote
                          ,(list (car spec) `((((type graphic))
                                               ,(cdr spec)))))))
    (reverse ret)))

(defconst mango-theme-colors-alist
  '((foreground       . "#C5C8C6")
    (background       . "#2B303B")
    (background-cool  . "#363C4A")
    (background-dark  . "#1D2635")
    (background-faint . "#414859")
    (middleground     . "#4F5561")
    (disabled         . "#999999")
    (pale-azure       . "#9CDCFE")
    (celestial-blue   . "#569CD6")
    (violet           . "#E57AE5")
    (khaki            . "#F0E68C")
    (lime             . "#B8F182")
    (orange           . "#F1B282")
    (pink             . "#ED97F5")
    (spanish-red      . "#E60026"))
  "The color palette used throughout the `mango-theme'.")

(mango-theme--generate-set-faces
  ;; Standard Stuff
  (default
   :foreground ,(mango-theme--color 'foreground)
   :background ,(mango-theme--color 'background))
  (fringe
   :inherit default)

  ;; Lines
  (hl-line
   :background ,(mango-theme--color 'background-faint))
  (region
   :background ,(mango-theme--color 'middleground))
  (header-line
   :background ,(mango-theme--color 'middleground))
  (mode-line-active
   :inherit header-line)
  (mode-line-inactive
   :background ,(mango-theme--color 'background-cool)
   :weight light)
  (window-divider
   :foreground ,(mango-theme--color 'background-cool))
  (window-divider-first-pixel
   :foreground ,(mango-theme--color 'background-cool))
  (window-divider-last-pixel
   :foreground ,(mango-theme--color 'background-cool))

  ;; Line Numbers
  (line-number
   :foreground ,(mango-theme--color 'background-faint)
   :background ,(mango-theme--color 'background))
  (line-number-current-line
   :foreground ,(mango-theme--color 'orange)
   :background ,(mango-theme--color 'background)
   :weight bold)

  ;; Documentation
  (font-lock-comment-face
   :foreground ,(mango-theme--color 'disabled))
  (font-lock-doc-face
   :inherit font-lock-comment-face)

  ;; Core Language
  (font-lock-keyword-face
   :foreground ,(mango-theme--color 'violet))
  (font-lock-type-face
   :foreground ,(mango-theme--color 'celestial-blue))
  (font-lock-builtin-face
   :inherit font-lock-preprocessor-face)

  ;; Function-likes
  (font-lock-function-name-face
   :foreground ,(mango-theme--color 'khaki))
  (font-lock-preprocessor-face
   :foreground ,(mango-theme--color 'pink)
   :weight bold)

  ;; Variables
  (font-lock-variable-name-face
   :foreground ,(mango-theme--color 'pale-azure))
  (font-lock-constant-face
   :inherit font-lock-variable-name-face
   :weight bold)

  ;; Other literals
  (font-lock-number-face
   :foreground ,(mango-theme--color 'orange))
  (help-key-binding
   :inherit font-lock-constant-face)

  ;; Org Mode
  (org-code
   :foreground ,(mango-theme--color 'orange))
  (org-verbatim
   :foreground ,(mango-theme--color 'lime))
  (org-block
   :background ,(mango-theme--color 'background-cool))
  (org-hide
   :foreground ,(mango-theme--color 'background))
  (org-quote
   :inherit org-block
   :slant italic)

  ;; Info Page
  (Info-quoted
   :inherit default)

  ;; Magit
  (magit-diff-hunk-heading
   :background ,(mango-theme--color 'background-cool))
  (magit-diff-hunk-heading-highlight
   :background ,(mango-theme--color 'middleground))
  (magit-diff-context-highlight
   :inherit hl-line)
  (magit-section-highlight
   :inherit hl-line)

  (git-commit-summary
   :foreground ,(mango-theme--color 'khaki))
  (git-commit-overlong-summary
   :foreground ,(mango-theme--color 'foreground)
   :background ,(mango-theme--color 'spanish-red)
   :weight bold)

  ;; Vertico
  (vertico-current
   :inherit hl-line)

  ;; Marginalia
  (marginalia-documentation
   :foreground ,(mango-theme--color 'disabled)
   :underline nil)

  ;; Tempel
  (tempel-default
   :slant italic
   :background ,(mango-theme--color 'middleground))
  (tempel-field
   :slant italic
   :background ,(mango-theme--color 'middleground))
  (tempel-form
   :slant italic
   :background ,(mango-theme--color 'middleground)))
