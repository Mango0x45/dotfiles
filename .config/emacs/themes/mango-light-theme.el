;;; mango-light-theme.el --- Just your average light theme  -*- lexical-binding: t; -*-

(deftheme mango-light
  "Mildly light, light theme.
Your average not-so-light light theme, because none of the other options
were exactly to my liking.  It’s about time I had a theme to call my
own.")

(defconst mango-light-theme-colors-alist
  '((foreground       . ("#3B4252" "color-238" "black"))
    (background       . ("#ECEFF4" "color-255" "white"))
    (background-cool  . ("#E5E9F0" "color-254" "white"))
    (background-dark  . ("#FAFBFC" "color-231" "brightwhite"))
    (background-faint . ("#D8DEE9" "color-253" "brightwhite"))
    (middleground     . ("#C8D0E0" "color-252" "brightwhite"))
    (disabled         . ("#9BA6B5" "color-247" "brightblack"))
    (celestial-blue   . ("#1B61CE" "color-26"  "blue"))
    (dark-red         . ("#A12027" "color-124" "red"))
    (khaki            . ("#8A6C23" "color-94"  "yellow"))
    (lime             . ("#358A2A" "color-28"  "green"))
    (magenta          . ("#9A35B3" "color-127" "magenta"))
    (pale-azure       . ("#0A74B8" "color-31"  "cyan"))
    (red              . ("#D22129" "color-160" "brightred"))
    (salmon           . ("#D1570B" "color-166" "brightyellow"))
    (violet           . ("#7A3B9E" "color-97"  "magenta")))
  "The color palette used throughout `mango-light-theme'.
Each color is mapped to a list of colors of the form
(GUI-HEX 256-COLOR 16-COLOR) for use in true-color, 256-color, and
16-color modes.")

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
     :foreground 'foreground
     :background 'background))
 `(fringe
   ((t (:inherit default))))

 ;; Lines
 `(hl-line
   ,(mango-light-theme-spec
     :background 'background-faint))
 `(region
   ,(mango-light-theme-spec
     :background 'middleground))
 `(header-line
   ,(mango-light-theme-spec
     :background 'middleground))
 `(mode-line-active
   ((t ( :box ,(mango-light-theme-color 'foreground 'gui)
         :inherit header-line))))
 `(mode-line-inactive
   ,(mango-light-theme-spec
     :background 'background-cool
     :weight 'light))
 `(window-divider
   ,(mango-light-theme-spec
     :foreground 'background-cool))
 `(window-divider-first-pixel
   ,(mango-light-theme-spec
     :foreground 'background-cool))
 `(window-divider-last-pixel
   ,(mango-light-theme-spec
     :foreground 'background-cool))

 ;; Line Numbers
 `(line-number
   ,(mango-light-theme-spec
     :foreground 'background-faint
     :background 'background))
 `(line-number-current-line
   ,(mango-light-theme-spec
     :foreground 'salmon
     :background 'background
     :weight 'bold))

 ;; Documentation
 `(font-lock-comment-face
   ,(mango-light-theme-spec
     :foreground 'khaki
     :weight 'semi-bold))
 `(font-lock-doc-face
   ,(mango-light-theme-spec
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
   ,(mango-light-theme-spec
     :background 'dark-red
     :box 'dark-red
     :weight 'bold))

 ;; Core Language
 `(font-lock-builtin-face
   ((t (:inherit font-lock-preprocessor-face))))
 `(font-lock-keyword-face
   ,(mango-light-theme-spec
     :foreground 'violet))
 `(font-lock-type-face
   ,(mango-light-theme-spec
     :foreground 'celestial-blue))

 ;; Function-likes
 `(font-lock-function-name-face
   ,(mango-light-theme-spec
     :foreground 'khaki))
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
     :foreground 'pale-azure))

 ;; Other literals
 `(help-key-binding
   ((t (:inherit font-lock-constant-face))))
 `(font-lock-number-face
   ,(mango-light-theme-spec
     :foreground 'salmon))

 ;; Org Mode
 `(org-quote
   ((t ( :inherit org-block
         :slant italic))))
 `(org-code
   ,(mango-light-theme-spec
     :foreground 'salmon))
 `(org-verbatim
   ,(mango-light-theme-spec
     :foreground 'lime))
 `(org-block
   ,(mango-light-theme-spec
     :background 'background-cool))
 `(org-hide
   ,(mango-light-theme-spec
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
   ,(mango-light-theme-spec
     :background 'background-cool))
 `(magit-diff-hunk-heading-highlight
   ,(mango-light-theme-spec
     :background 'middleground))
 `(git-commit-summary
   ,(mango-light-theme-spec
     :foreground 'khaki))
 `(git-commit-overlong-summary
   ,(mango-light-theme-spec
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
   ,(mango-light-theme-spec
     :foreground 'disabled
     :underline nil))

 ;; Tempel
 `(tempel-default
   ,(mango-light-theme-spec
     :slant 'italic
     :background 'middleground))
 `(tempel-field
   ,(mango-light-theme-spec
     :slant 'italic
     :background 'middleground))
 `(tempel-form
   ,(mango-light-theme-spec
     :slant 'italic
     :background 'middleground)))

(provide-theme 'mango-light)
