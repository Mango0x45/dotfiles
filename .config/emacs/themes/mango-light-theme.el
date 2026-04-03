;;; mango-light-theme.el --- Just your average light theme  -*- lexical-binding: t; -*-

(deftheme mango-light
  "Mildly light, light theme.
Your average not-so-light light theme, because none of the other options
were exactly to my liking.  It’s about time I had a theme to call my
own.")

(defconst mango-light-theme-colors-alist
  '((fg-main   . ("#2E3440" "color-237" "black"))
    (fg-muted  . ("#6B7A8F" "color-243" "brightblack"))
    (bg-main   . ("#FAFBFC" "color-231" "brightwhite"))
    (bg-alt    . ("#ECEFF4" "color-255" "white"))
    (bg-dim    . ("#E5E9F0" "color-254" "white"))
    (bg-hl     . ("#D8DEE9" "color-253" "brightwhite"))
    (bg-region . ("#C8D0E0" "color-252" "brightwhite"))
    (blue      . ("#1856B8" "color-26"  "blue"))
    (cyan      . ("#006B8F" "color-31"  "cyan"))
    (green     . ("#286620" "color-28"  "green"))
    (yellow    . ("#735610" "color-94"  "yellow"))
    (orange    . ("#B84C09" "color-166" "brightyellow"))
    (red       . ("#B51A22" "color-160" "brightred"))
    (red-dark  . ("#A12027" "color-124" "red"))
    (magenta   . ("#8828A1" "color-127" "magenta"))
    (violet    . ("#6B338A" "color-97"  "magenta")))
  "The color palette used throughout `mango-light-theme'.
Colors are grouped functionally for structured assignment across faces.")

(defsubst mango-light-theme-color (name &optional display)
  "Get the color value of NAME for the given DISPLAY.
DISPLAY can be 'gui, '256, or '16."
  (let ((colors (alist-get name mango-light-theme-colors-alist)))
    (pcase display
      ('gui (nth 0 colors))
      ('256 (nth 1 colors))
      ('16  (nth 2 colors))
      (_    (nth 0 colors)))))

(defun mango-light-theme-spec (&rest props)
  "Generate a tiered display specification list from PROPS.
Values that match keys in `mango-light-theme-colors-alist' are
automatically mapped to their correct display colors."
  (let (gui c256 c16)
    (while props
      (let ((key (pop props))
            (val (pop props)))
        (push key gui)
        (push key c256)
        (push key c16)
        (if (and (symbolp val) (alist-get val mango-light-theme-colors-alist))
            (progn
              (push (mango-light-theme-color val 'gui) gui)
              (push (mango-light-theme-color val '256) c256)
              (push (mango-light-theme-color val '16)  c16))
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
 'mango-light

 ;; Standard Stuff
 `(default
   ,(mango-light-theme-spec
     :foreground 'fg-main
     :background 'bg-main))
 `(fringe
   ((t (:inherit default))))

 ;; Modeline
 `(mm-modeline-modified
   ,(mango-light-theme-spec
     :foreground 'red
     :weight 'bold))
 `(mm-modeline-read-only
   ,(mango-light-theme-spec
     :foreground 'yellow
     :weight 'bold))
 `(mm-modeline-narrowed
   ,(mango-light-theme-spec
     :foreground 'blue
     :weight 'bold))
 `(mm-modeline-overwrite
   ,(mango-light-theme-spec
     :foreground 'magenta
     :weight 'bold))
 `(mm-modeline-region
   ,(mango-light-theme-spec
     :foreground 'fg-main
     :background 'bg-region
     :weight 'bold))
 `(mm-modeline-position
   ,(mango-light-theme-spec
     :foreground 'cyan
     :weight 'bold))
 `(mm-modeline-major-mode
   ,(mango-light-theme-spec
     :foreground 'violet
     :weight 'bold))
 `(mm-modeline-vc
   ,(mango-light-theme-spec
     :foreground 'green
     :weight 'bold))

 ;; Tab Bar
 `(tab-bar
   ,(mango-light-theme-spec
     :background 'bg-alt
     :foreground 'fg-main))
 `(tab-bar-tab
   ,(mango-light-theme-spec
     :background 'bg-main
     :foreground 'fg-main
     :weight 'bold
     :underline `( :color ,(mango-light-theme-color 'blue 'gui)
                   :style line
                   :position -1)))
 `(tab-bar-tab-inactive
   ,(mango-light-theme-spec
     :background 'bg-dim
     :foreground 'fg-muted))

 ;; Lines
 `(hl-line
   ,(mango-light-theme-spec
     :background 'bg-hl))
 `(region
   ,(mango-light-theme-spec
     :background 'bg-region))
 `(header-line
   ,(mango-light-theme-spec
     :background 'bg-region))
 `(mode-line-active
   ((t ( :box ,(mango-light-theme-color 'fg-main 'gui)
         :inherit header-line))))
 `(mode-line-inactive
   ,(mango-light-theme-spec
     :background 'bg-dim
     :box 'bg-hl
     :weight 'light))
 `(window-divider
   ,(mango-light-theme-spec
     :foreground 'bg-dim))
 `(window-divider-first-pixel
   ,(mango-light-theme-spec
     :foreground 'bg-dim))
 `(window-divider-last-pixel
   ,(mango-light-theme-spec
     :foreground 'bg-dim))

 ;; Line Numbers
 `(line-number
   ,(mango-light-theme-spec
     :foreground 'bg-hl
     :background 'bg-main))
 `(line-number-current-line
   ,(mango-light-theme-spec
     :foreground 'orange
     :background 'bg-main
     :weight 'bold))

 ;; Documentation
 `(font-lock-comment-face
   ,(mango-light-theme-spec
     :foreground 'yellow
     :weight 'semi-bold))
 `(font-lock-doc-face
   ,(mango-light-theme-spec
     :foreground 'fg-muted))

 ;; Core Language
 `(font-lock-builtin-face
   ((t (:inherit font-lock-preprocessor-face))))
 `(font-lock-keyword-face
   ,(mango-light-theme-spec
     :foreground 'violet))
 `(font-lock-type-face
   ,(mango-light-theme-spec
     :foreground 'blue))

 ;; Function-likes
 `(font-lock-function-name-face
   ,(mango-light-theme-spec
     :foreground 'yellow))
 `(font-lock-preprocessor-face
   ,(mango-light-theme-spec
     :foreground 'magenta
     :weight 'bold))

 ;; Variables
 `(font-lock-constant-face
   ((t ( :inherit font-lock-variable-name-face
         :weight bold))))
 `(font-lock-variable-name-face
   ,(mango-light-theme-spec
     :foreground 'cyan))

 ;; Other literals
 `(help-key-binding
   ((t (:inherit font-lock-constant-face))))
 `(font-lock-number-face
   ,(mango-light-theme-spec
     :foreground 'orange))

 ;; Org Mode
 `(org-quote
   ((t ( :inherit org-block
         :slant italic))))
 `(org-code
   ,(mango-light-theme-spec
     :foreground 'orange))
 `(org-verbatim
   ,(mango-light-theme-spec
     :foreground 'green))
 `(org-block
   ,(mango-light-theme-spec
     :background 'bg-dim))
 `(org-hide
   ,(mango-light-theme-spec
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
   ,(mango-light-theme-spec
     :background 'bg-dim))
 `(magit-diff-hunk-heading-highlight
   ,(mango-light-theme-spec
     :background 'bg-region))
 `(git-commit-summary
   ,(mango-light-theme-spec
     :foreground 'yellow))
 `(git-commit-overlong-summary
   ,(mango-light-theme-spec
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
   ,(mango-light-theme-spec
     :foreground 'fg-muted
     :underline nil))

 ;; Tempel
 `(tempel-default
   ,(mango-light-theme-spec
     :slant 'italic
     :background 'bg-region))
 `(tempel-field
   ,(mango-light-theme-spec
     :slant 'italic
     :background 'bg-region))
 `(tempel-form
   ,(mango-light-theme-spec
     :slant 'italic
     :background 'bg-region)))

(provide-theme 'mango-light)
