;;; mango-dark-theme.el --- Just your average dark theme  -*- lexical-binding: t; -*-

(deftheme mango-dark
  "Mildly dark, dark theme.
Your average not-so-dark dark theme, because none of the other options
were exactly to my liking.  It’s about time I had a theme to call my
own.")

(defconst mango-dark-theme-colors-alist
  '((foreground       . ("#C5C8C6" "color-251" "white"))
    (background       . ("#2B303B" "color-236" "black"))
    (background-cool  . ("#363C4A" "color-237" "black"))
    (background-dark  . ("#1D2635" "color-234" "black"))
    (background-faint . ("#414859" "color-238" "brightblack"))
    (middleground     . ("#4F5561" "color-239" "brightblack"))
    (disabled         . ("#999999" "color-246" "brightblack"))
    (celestial-blue   . ("#569CD6" "color-74"  "brightblue"))
    (dark-red         . ("#841A11" "color-88"  "red"))
    (khaki            . ("#F0E68C" "color-228" "yellow"))
    (lime             . ("#B8F182" "color-156" "green"))
    (magenta          . ("#ED97F5" "color-213" "magenta"))
    (pale-azure       . ("#9CDCFE" "color-117" "cyan"))
    (red              . ("#E60026" "color-160" "brightred"))
    (salmon           . ("#F1B282" "color-216" "brightyellow"))
    (violet           . ("#E57AE5" "color-176" "brightmagenta")))
  "The color palette used throughout `mango-dark-theme'.
Each color is mapped to a list of colors of the form
(GUI-HEX 256-COLOR 16-COLOR) for use in true-color, 256-color, and
16-color modes.")

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
     :foreground 'foreground
     :background 'background))
 `(fringe
   ((t (:inherit default))))

 ;; Lines
 `(hl-line
   ,(mango-dark-theme-spec
     :background 'background-faint))
 `(region
   ,(mango-dark-theme-spec
     :background 'middleground))
 `(header-line
   ,(mango-dark-theme-spec
     :background 'middleground))
 `(mode-line-active
   ((t ( :box ,(mango-dark-theme-color 'foreground 'gui)
         :inherit header-line))))
 `(mode-line-inactive
   ,(mango-dark-theme-spec
     :background 'background-cool
     :weight 'light))
 `(window-divider
   ,(mango-dark-theme-spec
     :foreground 'background-cool))
 `(window-divider-first-pixel
   ,(mango-dark-theme-spec
     :foreground 'background-cool))
 `(window-divider-last-pixel
   ,(mango-dark-theme-spec
     :foreground 'background-cool))

 ;; Line Numbers
 `(line-number
   ,(mango-dark-theme-spec
     :foreground 'background-faint
     :background 'background))
 `(line-number-current-line
   ,(mango-dark-theme-spec
     :foreground 'salmon
     :background 'background
     :weight 'bold))

 ;; Documentation
 `(font-lock-comment-face
   ,(mango-dark-theme-spec
     :foreground 'khaki
     :weight 'semi-bold))
 `(font-lock-doc-face
   ,(mango-dark-theme-spec
     :foreground 'disabled))

 ;; Modeline
 `(mm-modeline-overwrite-face
   ((t (:weight bold))))
 `(mm-modeline-readonly-face
   ((t (:weight bold))))
 `(mm-modeline-buffer-name-face
   ((t (:inherit font-lock-constant-face))))
 `(mm-modeline-buffer-modified-face
   ((t (:inherit shadow))))
 `(mm-modeline-major-mode-name-face
   ((t (:weight bold))))
 `(mm-modeline-major-mode-symbol-face
   ((t (:inherit shadow))))
 `(mm-modeline-git-branch-face
   ((t (:inherit font-lock-constant-face))))
 `(mm-modeline-narrow-face
   ,(mango-dark-theme-spec
     :background 'dark-red
     :box 'dark-red
     :weight 'bold))

 ;; Core Language
 `(font-lock-builtin-face
   ((t (:inherit font-lock-preprocessor-face))))
 `(font-lock-keyword-face
   ,(mango-dark-theme-spec
     :foreground 'violet))
 `(font-lock-type-face
   ,(mango-dark-theme-spec
     :foreground 'celestial-blue))

 ;; Function-likes
 `(font-lock-function-name-face
   ,(mango-dark-theme-spec
     :foreground 'khaki))
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
     :foreground 'pale-azure))

 ;; Other literals
 `(help-key-binding
   ((t (:inherit font-lock-constant-face))))
 `(font-lock-number-face
   ,(mango-dark-theme-spec
     :foreground 'salmon))

 ;; Org Mode
 `(org-quote
   ((t ( :inherit org-block
         :slant italic))))
 `(org-code
   ,(mango-dark-theme-spec
     :foreground 'salmon))
 `(org-verbatim
   ,(mango-dark-theme-spec
     :foreground 'lime))
 `(org-block
   ,(mango-dark-theme-spec
     :background 'background-cool))
 `(org-hide
   ,(mango-dark-theme-spec
     :foreground 'background))

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
     :background 'background-cool))
 `(magit-diff-hunk-heading-highlight
   ,(mango-dark-theme-spec
     :background 'middleground))
 `(git-commit-summary
   ,(mango-dark-theme-spec
     :foreground 'khaki))
 `(git-commit-overlong-summary
   ,(mango-dark-theme-spec
     :foreground 'foreground
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
     :foreground 'disabled
     :underline nil))

 ;; Tempel
 `(tempel-default
   ,(mango-dark-theme-spec
     :slant 'italic
     :background 'middleground))
 `(tempel-field
   ,(mango-dark-theme-spec
     :slant 'italic
     :background 'middleground))
 `(tempel-form
   ,(mango-dark-theme-spec
     :slant 'italic
     :background 'middleground)))

(provide-theme 'mango-dark)
