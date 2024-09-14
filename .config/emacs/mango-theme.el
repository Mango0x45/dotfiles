;;; mango-theme.el --- Just your average dark theme  -*- lexical-binding: t; -*-

(deftheme mango
  "Just another dark theme because none of the other options out there were just
as I would like them.  Why try to fix someone elses themes when I make my own?")

(defun mango-theme--get-color (name)
  "Get the RGB value of the color NAME from ‘mango-theme-palette’"
  (cadr (assq name mango-theme-palette)))

(defmacro mango-theme--generate-set-faces (&rest body)
  "A macro to provide a much simpler syntax than what is expected by
‘custom-theme-set-faces’.  This is possible because I only run Emacs
graphically, so I shouldn’t need to have multiple specs per face.

\(fn SPEC...)"
  (declare (indent 0))
  (let ((ret '('mango custom-theme-set-faces)))
    (dolist (spec body)
      (add-to-list 'ret `(backquote ,(list (car spec) `((((type graphic)) ,(cdr spec)))))))
    (reverse ret)))

(defconst mango-theme-palette
  '((foreground       "#C5C8C6")
    (background       "#2B303B")
    (background-cool  "#363C4A")
    (background-faint "#414859")
    (middleground     "#4F5561")
    (disabled         "#999999")
    (pale-azure       "#9CDCFE")
    (celestial-blue   "#569CD6")
    (violet           "#E57AE5")
    (khaki            "#F0E68C")
    (lime             "#B8F182")
    (orange           "#F1B282")
    (pink             "#ED97F5")
    (spanish-red      "#E60026"))
  "The color palette used throughout the ‘mango’ theme.")

(mango-theme--generate-set-faces
  ;; Standard Stuff
  (default
   :foreground ,(mango-theme--get-color 'foreground)
   :background ,(mango-theme--get-color 'background))
  (fringe
   :inherit default)

  ;; Lines
  (hl-line
   :background ,(mango-theme--get-color 'background-faint))
  (region
   :background ,(mango-theme--get-color 'middleground))
  (header-line
   :background ,(mango-theme--get-color 'middleground))
  (mode-line
   :inherit header-line)
  (mode-line-inactive
   :background ,(mango-theme--get-color 'background-cool)
   :weight light)

  ;; Line Numbers
  (line-number
   :foreground ,(mango-theme--get-color 'background-faint)
   :background ,(mango-theme--get-color 'background))
  (line-number-current-line
   :foreground ,(mango-theme--get-color 'orange)
   :background ,(mango-theme--get-color 'background)
   :weight bold)

  ;; Documentation
  (font-lock-comment-face
   :foreground ,(mango-theme--get-color 'disabled))
  (font-lock-doc-face
   :inherit font-lock-comment-face)

  ;; Core Language
  (font-lock-keyword-face
   :foreground ,(mango-theme--get-color 'violet))
  (font-lock-type-face
   :foreground ,(mango-theme--get-color 'celestial-blue))
  (font-lock-builtin-face
   :inherit font-lock-preprocessor-face)

  ;; Function-likes
  (font-lock-function-name-face
   :foreground ,(mango-theme--get-color 'khaki))
  (font-lock-preprocessor-face
   :foreground ,(mango-theme--get-color 'pink)
   :weight bold)

  ;; Variables
  (font-lock-variable-name-face
   :foreground ,(mango-theme--get-color 'pale-azure))
  (font-lock-constant-face
   :inherit font-lock-variable-name-face
   :weight bold)

  ;; Other literals
  (font-lock-number-face
   :foreground ,(mango-theme--get-color 'orange))

  ;; Org Mode
  (org-code
   :foreground ,(mango-theme--get-color 'orange))
  (org-verbatim
   :foreground ,(mango-theme--get-color 'lime))
  (org-block
   :background ,(mango-theme--get-color 'background-cool))
  (org-hide
   :foreground ,(mango-theme--get-color 'background))
  (org-quote
   :inherit org-block
   :slant italic)

  ;; Info Page
  (Info-quoted
   :inherit default)

  ;; Magit
  (magit-diff-hunk-heading
   :background ,(mango-theme--get-color 'background-cool))
  (magit-diff-hunk-heading-highlight
   :background ,(mango-theme--get-color 'middleground))
  (magit-diff-context-highlight
   :inherit hl-line)
  (magit-section-highlight
   :inherit hl-line)

  (git-commit-summary
   :foreground ,(mango-theme--get-color 'khaki))
  (git-commit-overlong-summary
   :foreground ,(mango-theme--get-color 'foreground)
   :background ,(mango-theme--get-color 'spanish-red)
   :weight bold)

  ;; Vertico
  (vertico-current
   :inherit hl-line)

  ;; Marginalia
  (marginalia-documentation
   :foreground ,(mango-theme--get-color 'disabled)
   :underline nil)

  ;; Tempel
  (tempel-default
   :slant italic
   :background ,(mango-theme--get-color 'middleground))
  (tempel-field
   :slant italic
   :background ,(mango-theme--get-color 'middleground))
  (tempel-form
   :slant italic
   :background ,(mango-theme--get-color 'middleground)))
