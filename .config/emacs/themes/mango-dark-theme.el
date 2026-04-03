;;; mango-dark-theme.el --- Just your average dark theme  -*- lexical-binding: t; -*-

(deftheme mango-dark
  "Mildly dark, dark theme.
Your average not-so-dark dark theme, because none of the other options
were exactly to my liking.  It’s about time I had a theme to call my
own.")

(defconst mango-dark-theme-colors-alist
  '((fg-main   . ("#D1D5D8" "color-251" "white"))
    (fg-muted  . ("#939CA8" "color-246" "brightblack"))
    (bg-dim    . ("#1D232F" "color-234" "black"))
    (bg-main   . ("#2B303B" "color-236" "black"))
    (bg-alt    . ("#363C4A" "color-237" "black"))
    (bg-hl     . ("#414859" "color-238" "brightblack"))
    (bg-region . ("#4F5561" "color-239" "brightblack"))
    (blue      . ("#569CD6" "color-74"  "brightblue"))
    (cyan      . ("#7DC1E6" "color-117" "cyan"))
    (green     . ("#A6E22E" "color-156" "green"))
    (yellow    . ("#E5D070" "color-228" "yellow"))
    (orange    . ("#ECA671" "color-216" "brightyellow"))
    (red       . ("#F24E4E" "color-160" "brightred"))
    (red-dark  . ("#A42A22" "color-88"  "red"))
    (magenta   . ("#E183E8" "color-213" "magenta"))
    (violet    . ("#C678DD" "color-176" "brightmagenta")))
  "The color palette used throughout `mango-dark-theme'.
Colors are grouped functionally for structured assignment across faces.")

(defsubst mango-dark-theme-color (name &optional display)
  "Get the color value of NAME for the given DISPLAY.
DISPLAY can be 'gui, '256, or '16."
  (let ((colors (alist-get name mango-dark-theme-colors-alist)))
    (pcase display
      ('gui (nth 0 colors))
      ('256 (nth 1 colors))
      ('16  (nth 2 colors))
      (_    (nth 0 colors)))))

(defun mango-dark-theme-spec (&rest props)
  "Generate a tiered display specification list from PROPS.
Values that match keys in `mango-dark-theme-colors-alist' are
automatically mapped to their correct display colors."
  (let (gui c256 c16)
    (while props
      (let ((key (pop props))
            (val (pop props)))
        (push key gui)
        (push key c256)
        (push key c16)
        (if (and (symbolp val) (alist-get val mango-dark-theme-colors-alist))
            (progn
              (push (mango-dark-theme-color val 'gui) gui)
              (push (mango-dark-theme-color val '256) c256)
              (push (mango-dark-theme-color val '16)  c16))
          (push val gui)
          (push val c256)
          (push val c16))))
    `((((type graphic tty) (min-colors #x1000000))
       ,(nreverse gui))
      (((type tty) (min-colors 256))
       ,(nreverse c256))
      (((type tty))
       ,(nreverse c16)))))

(custom-theme-set-faces
 'mango-dark

 ;; Standard Stuff
 `(default
   ,(mango-dark-theme-spec
     :foreground 'fg-main
     :background 'bg-main))
 `(fringe
   ((t (:inherit default))))

 ;; Modeline
 `(mm-modeline-modified
   ,(mango-dark-theme-spec
     :foreground 'red
     :weight 'bold))
 `(mm-modeline-read-only
   ,(mango-dark-theme-spec
     :foreground 'yellow
     :weight 'semi-bold))
 `(mm-modeline-narrowed
   ,(mango-dark-theme-spec
     :foreground 'green
     :weight 'bold))
 `(mm-modeline-overwrite
   ,(mango-dark-theme-spec
     :foreground 'magenta
     :weight 'bold))
 `(mm-modeline-region
   ,(mango-dark-theme-spec
     :background 'bg-region
     :weight 'bold))
 `(mm-modeline-position
   ,(mango-dark-theme-spec
     :foreground 'cyan
     :weight 'bold))
 `(mm-modeline-major-mode
   ,(mango-dark-theme-spec
     :foreground 'violet
     :weight 'bold))
 `(mm-modeline-vc
   ,(mango-dark-theme-spec
     :foreground 'green
     :weight 'bold))

 ;; Tab Bar
 `(tab-bar
   ,(mango-dark-theme-spec
     :background 'bg-alt
     :foreground 'fg-main))
 `(tab-bar-tab
   ,(mango-dark-theme-spec
     :background 'bg-main
     :foreground 'fg-main
     :weight 'bold
     :underline `( :color ,(mango-dark-theme-color 'blue 'gui)
                   :style line
                   :position -1)))
 `(tab-bar-tab-inactive
   ,(mango-dark-theme-spec
     :background 'bg-dim
     :foreground 'fg-muted))

 ;; Lines
 `(hl-line
   ,(mango-dark-theme-spec
     :background 'bg-hl))
 `(region
   ,(mango-dark-theme-spec
     :background 'bg-region))
 `(header-line
   ,(mango-dark-theme-spec
     :background 'bg-region))
 `(mode-line-active
   ((t ( :box ,(mango-dark-theme-color 'fg-main 'gui)
         :inherit header-line))))
 `(mode-line-inactive
   ,(mango-dark-theme-spec
     :background 'bg-alt
     :box 'bg-hl
     :weight 'light))
 `(window-divider
   ,(mango-dark-theme-spec
     :foreground 'bg-alt))
 `(window-divider-first-pixel
   ,(mango-dark-theme-spec
     :foreground 'bg-alt))
 `(window-divider-last-pixel
   ,(mango-dark-theme-spec
     :foreground 'bg-alt))

 ;; Line Numbers
 `(line-number
   ,(mango-dark-theme-spec
     :foreground 'bg-hl
     :background 'bg-main))
 `(line-number-current-line
   ,(mango-dark-theme-spec
     :foreground 'orange
     :background 'bg-main
     :weight 'bold))

 ;; Documentation
 `(font-lock-comment-face
   ,(mango-dark-theme-spec
     :foreground 'yellow
     :weight 'semi-bold))
 `(font-lock-doc-face
   ,(mango-dark-theme-spec
     :foreground 'fg-muted))

 ;; Core Language
 `(font-lock-builtin-face
   ((t (:inherit font-lock-preprocessor-face))))
 `(font-lock-keyword-face
   ,(mango-dark-theme-spec
     :foreground 'violet))
 `(font-lock-type-face
   ,(mango-dark-theme-spec
     :foreground 'blue))

 ;; Function-likes
 `(font-lock-function-name-face
   ,(mango-dark-theme-spec
     :foreground 'yellow))
 `(font-lock-preprocessor-face
   ,(mango-dark-theme-spec
     :foreground 'magenta
     :weight 'bold))

 ;; Variables
 `(font-lock-constant-face
   ((t ( :inherit font-lock-variable-name-face
         :weight bold))))
 `(font-lock-variable-name-face
   ,(mango-dark-theme-spec
     :foreground 'cyan))

 ;; Other literals
 `(help-key-binding
   ((t (:inherit font-lock-constant-face))))
 `(font-lock-number-face
   ,(mango-dark-theme-spec
     :foreground 'orange))

 ;; Org Mode
 `(org-quote
   ((t ( :inherit org-block
         :slant italic))))
 `(org-code
   ,(mango-dark-theme-spec
     :foreground 'orange))
 `(org-verbatim
   ,(mango-dark-theme-spec
     :foreground 'green))
 `(org-block
   ,(mango-dark-theme-spec
     :background 'bg-alt))
 `(org-hide
   ,(mango-dark-theme-spec
     :foreground 'bg-main))

 ;; Info Page
 `(Info-quoted
   ((t (:inherit default))))

 ;; Magit
 `(magit-diff-context-highlight
   ((t (:inherit hl-line))))
 `(magit-section-highlight
   ((t (:inherit hl-line))))
 `(magit-diff-hunk-heading
   ,(mango-dark-theme-spec
     :background 'bg-alt))
 `(magit-diff-hunk-heading-highlight
   ,(mango-dark-theme-spec
     :background 'bg-region))
 `(git-commit-summary
   ,(mango-dark-theme-spec
     :foreground 'yellow))
 `(git-commit-overlong-summary
   ,(mango-dark-theme-spec
     :foreground 'fg-main
     :background 'red
     :weight 'bold))

 ;; Vertico
 `(vertico-current ((t (:inherit hl-line))))

 ;; Marginalia
 `(mm-diffstat-counter-added
   ((t ( :foreground "green"
         :weight bold))))
 `(mm-diffstat-counter-removed
   ((t ( :foreground "red"
         :weight bold))))
 `(marginalia-documentation
   ,(mango-dark-theme-spec
     :foreground 'fg-muted
     :underline nil))

 ;; Tempel
 `(tempel-default
   ,(mango-dark-theme-spec
     :slant 'italic
     :background 'bg-region))
 `(tempel-field
   ,(mango-dark-theme-spec
     :slant 'italic
     :background 'bg-region))
 `(tempel-form
   ,(mango-dark-theme-spec
     :slant 'italic
     :background 'bg-region)))

(provide-theme 'mango-dark)
