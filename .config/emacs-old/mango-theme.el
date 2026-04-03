;;; mango-theme.el --- Just your average dark theme  -*- lexical-binding: t; -*-

(deftheme mango
  "Mildly dark, dark theme.
Your average not-so-dark dark theme, because none of the other options
were exactly to my liking.  It’s about time I had a theme to call my
own.")

(defconst mango-theme-colors-alist
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
  "The color palette used throughout `mango-theme'.
Each color is mapped to a list of colors of the form
(GUI-HEX 256-COLOR 16-COLOR) for use in true-color, 256-color, and
16-color modes.")

(defsubst mango-theme-color (name &optional display)
  "Get the color value of NAME for the given DISPLAY.
DISPLAY can be 'gui, '256, or '16."
  (let ((colors (alist-get name mango-theme-colors-alist)))
    (pcase display
      ('gui (nth 0 colors))
      ('256 (nth 1 colors))
      ('16  (nth 2 colors))
      (_    (nth 0 colors)))))

(custom-theme-set-faces
 'mango

 ;; Standard Stuff
 `(default
   ((((type graphic tty) (min-colors 16777216))
     (:foreground ,(mango-theme-color 'foreground 'gui)
      :background ,(mango-theme-color 'background 'gui)))
    (((type tty) (min-colors 256))
     (:foreground ,(mango-theme-color 'foreground '256)
      :background ,(mango-theme-color 'background '256)))
    (((type tty))
     (:foreground ,(mango-theme-color 'foreground '16)
      :background ,(mango-theme-color 'background '16)))))
 `(fringe
   ((t (:inherit default))))

 ;; Lines
 `(hl-line
   ((((type graphic tty) (min-colors 16777216))
     (:background ,(mango-theme-color 'background-faint 'gui)))
    (((type tty) (min-colors 256))
     (:background ,(mango-theme-color 'background-faint '256)))
    (((type tty))
     (:background ,(mango-theme-color 'background-faint '16)))))
 `(region
   ((((type graphic tty) (min-colors 16777216))
     (:background ,(mango-theme-color 'middleground 'gui)))
    (((type tty) (min-colors 256))
     (:background ,(mango-theme-color 'middleground '256)))
    (((type tty))
     (:background ,(mango-theme-color 'middleground '16)))))
 `(header-line
   ((((type graphic tty) (min-colors 16777216))
     (:background ,(mango-theme-color 'middleground 'gui)))
    (((type tty) (min-colors 256))
     (:background ,(mango-theme-color 'middleground '256)))
    (((type tty))
     (:background ,(mango-theme-color 'middleground '16)))))
 `(mode-line-active
   ((t (:inherit header-line))))
 `(mode-line-inactive
   ((((type graphic tty) (min-colors 16777216))
     (:background ,(mango-theme-color 'background-cool 'gui) :weight light))
    (((type tty) (min-colors 256))
     (:background ,(mango-theme-color 'background-cool '256) :weight light))
    (((type tty))
     (:background ,(mango-theme-color 'background-cool '16) :weight light))))
 `(window-divider
   ((((type graphic tty) (min-colors 16777216))
     (:foreground ,(mango-theme-color 'background-cool 'gui)))
    (((type tty) (min-colors 256))
     (:foreground ,(mango-theme-color 'background-cool '256)))
    (((type tty))
     (:foreground ,(mango-theme-color 'background-cool '16)))))
 `(window-divider-first-pixel
   ((((type graphic tty) (min-colors 16777216))
     (:foreground ,(mango-theme-color 'background-cool 'gui)))
    (((type tty) (min-colors 256))
     (:foreground ,(mango-theme-color 'background-cool '256)))
    (((type tty))
     (:foreground ,(mango-theme-color 'background-cool '16)))))
 `(window-divider-last-pixel
   ((((type graphic tty) (min-colors 16777216))
     (:foreground ,(mango-theme-color 'background-cool 'gui)))
    (((type tty) (min-colors 256))
     (:foreground ,(mango-theme-color 'background-cool '256)))
    (((type tty))
     (:foreground ,(mango-theme-color 'background-cool '16)))))

 ;; Line Numbers
 `(line-number
   ((((type graphic tty) (min-colors 16777216))
     (:foreground ,(mango-theme-color 'background-faint 'gui)
      :background ,(mango-theme-color 'background 'gui)))
    (((type tty) (min-colors 256))
     (:foreground ,(mango-theme-color 'background-faint '256)
      :background ,(mango-theme-color 'background '256)))
    (((type tty))
     (:foreground ,(mango-theme-color 'background-faint '16)
      :background ,(mango-theme-color 'background '16)))))
 `(line-number-current-line
   ((((type graphic tty) (min-colors 16777216))
     (:foreground ,(mango-theme-color 'salmon 'gui)
      :background ,(mango-theme-color 'background 'gui) :weight bold))
    (((type tty) (min-colors 256))
     (:foreground ,(mango-theme-color 'salmon '256)
      :background ,(mango-theme-color 'background '256) :weight bold))
    (((type tty))
     (:foreground ,(mango-theme-color 'salmon '16)
      :background ,(mango-theme-color 'background '16) :weight bold))))

 ;; Documentation
 `(font-lock-comment-face
   ((((type graphic tty) (min-colors 16777216))
     (:foreground ,(mango-theme-color 'khaki 'gui) :weight semi-bold))
    (((type tty) (min-colors 256))
     (:foreground ,(mango-theme-color 'khaki '256) :weight semi-bold))
    (((type tty))
     (:foreground ,(mango-theme-color 'khaki '16) :weight semi-bold))))
 `(font-lock-doc-face
   ((((type graphic tty) (min-colors 16777216))
     (:foreground ,(mango-theme-color 'disabled 'gui)))
    (((type tty) (min-colors 256))
     (:foreground ,(mango-theme-color 'disabled '256)))
    (((type tty))
     (:foreground ,(mango-theme-color 'disabled '16)))))

 ;; Modeline
 `(mm-modeline-overwrite-face ((t (:weight bold))))
 `(mm-modeline-readonly-face ((t (:weight bold))))
 `(mm-modeline-buffer-name-face ((t (:inherit font-lock-constant-face))))
 `(mm-modeline-buffer-modified-face ((t (:inherit shadow))))
 `(mm-modeline-major-mode-name-face ((t (:weight bold))))
 `(mm-modeline-major-mode-symbol-face ((t (:inherit shadow))))
 `(mm-modeline-git-branch-face ((t (:inherit font-lock-constant-face))))
 `(mm-modeline-narrow-face
   ((((type graphic tty) (min-colors 16777216))
     (:background ,(mango-theme-color 'dark-red 'gui) :box ,(mango-theme-color 'dark-red 'gui) :weight bold))
    (((type tty) (min-colors 256))
     (:background ,(mango-theme-color 'dark-red '256) :box ,(mango-theme-color 'dark-red '256) :weight bold))
    (((type tty))
     (:background ,(mango-theme-color 'dark-red '16) :box ,(mango-theme-color 'dark-red '16) :weight bold))))

 ;; Core Language
 `(font-lock-builtin-face ((t (:inherit font-lock-preprocessor-face))))
 `(font-lock-keyword-face
   ((((type graphic tty) (min-colors 16777216))
     (:foreground ,(mango-theme-color 'violet 'gui)))
    (((type tty) (min-colors 256))
     (:foreground ,(mango-theme-color 'violet '256)))
    (((type tty))
     (:foreground ,(mango-theme-color 'violet '16)))))
 `(font-lock-type-face
   ((((type graphic tty) (min-colors 16777216))
     (:foreground ,(mango-theme-color 'celestial-blue 'gui)))
    (((type tty) (min-colors 256))
     (:foreground ,(mango-theme-color 'celestial-blue '256)))
    (((type tty))
     (:foreground ,(mango-theme-color 'celestial-blue '16)))))

 ;; Function-likes
 `(font-lock-function-name-face
   ((((type graphic tty) (min-colors 16777216))
     (:foreground ,(mango-theme-color 'khaki 'gui)))
    (((type tty) (min-colors 256))
     (:foreground ,(mango-theme-color 'khaki '256)))
    (((type tty))
     (:foreground ,(mango-theme-color 'khaki '16)))))
 `(font-lock-preprocessor-face
   ((((type graphic tty) (min-colors 16777216))
     (:foreground ,(mango-theme-color 'magenta 'gui) :weight bold))
    (((type tty) (min-colors 256))
     (:foreground ,(mango-theme-color 'magenta '256) :weight bold))
    (((type tty))
     (:foreground ,(mango-theme-color 'magenta '16) :weight bold))))

 ;; Variables
 `(font-lock-constant-face ((t (:inherit font-lock-variable-name-face :weight bold))))
 `(font-lock-variable-name-face
   ((((type graphic tty) (min-colors 16777216))
     (:foreground ,(mango-theme-color 'pale-azure 'gui)))
    (((type tty) (min-colors 256))
     (:foreground ,(mango-theme-color 'pale-azure '256)))
    (((type tty))
     (:foreground ,(mango-theme-color 'pale-azure '16)))))

 ;; Other literals
 `(help-key-binding ((t (:inherit font-lock-constant-face))))
 `(font-lock-string-face
   ((((type graphic tty) (min-colors 16777216))
     (:foreground ,(mango-theme-color 'salmon 'gui)))
    (((type tty) (min-colors 256))
     (:foreground ,(mango-theme-color 'salmon '256)))
    (((type tty))
     (:foreground ,(mango-theme-color 'salmon '16)))))
 `(font-lock-number-face
   ((((type graphic tty) (min-colors 16777216))
     (:foreground ,(mango-theme-color 'salmon 'gui)))
    (((type tty) (min-colors 256))
     (:foreground ,(mango-theme-color 'salmon '256)))
    (((type tty))
     (:foreground ,(mango-theme-color 'salmon '16)))))

 ;; Org Mode
 `(org-quote ((t (:inherit org-block :slant italic))))
 `(org-code
   ((((type graphic tty) (min-colors 16777216))
     (:foreground ,(mango-theme-color 'salmon 'gui)))
    (((type tty) (min-colors 256))
     (:foreground ,(mango-theme-color 'salmon '256)))
    (((type tty))
     (:foreground ,(mango-theme-color 'salmon '16)))))
 `(org-verbatim
   ((((type graphic tty) (min-colors 16777216))
     (:foreground ,(mango-theme-color 'lime 'gui)))
    (((type tty) (min-colors 256))
     (:foreground ,(mango-theme-color 'lime '256)))
    (((type tty))
     (:foreground ,(mango-theme-color 'lime '16)))))
 `(org-block
   ((((type graphic tty) (min-colors 16777216))
     (:background ,(mango-theme-color 'background-cool 'gui)))
    (((type tty) (min-colors 256))
     (:background ,(mango-theme-color 'background-cool '256)))
    (((type tty))
     (:background ,(mango-theme-color 'background-cool '16)))))
 `(org-hide
   ((((type graphic tty) (min-colors 16777216))
     (:foreground ,(mango-theme-color 'background 'gui)))
    (((type tty) (min-colors 256))
     (:foreground ,(mango-theme-color 'background '256)))
    (((type tty))
     (:foreground ,(mango-theme-color 'background '16)))))

 ;; Info Page
 `(Info-quoted ((t (:inherit default))))

 ;; Magit
 `(magit-diff-context-highlight ((t (:inherit hl-line))))
 `(magit-section-highlight ((t (:inherit hl-line))))
 `(magit-diff-hunk-heading
   ((((type graphic tty) (min-colors 16777216))
     (:background ,(mango-theme-color 'background-cool 'gui)))
    (((type tty) (min-colors 256))
     (:background ,(mango-theme-color 'background-cool '256)))
    (((type tty))
     (:background ,(mango-theme-color 'background-cool '16)))))
 `(magit-diff-hunk-heading-highlight
   ((((type graphic tty) (min-colors 16777216))
     (:background ,(mango-theme-color 'middleground 'gui)))
    (((type tty) (min-colors 256))
     (:background ,(mango-theme-color 'middleground '256)))
    (((type tty))
     (:background ,(mango-theme-color 'middleground '16)))))
 `(git-commit-summary
   ((((type graphic tty) (min-colors 16777216))
     (:foreground ,(mango-theme-color 'khaki 'gui)))
    (((type tty) (min-colors 256))
     (:foreground ,(mango-theme-color 'khaki '256)))
    (((type tty))
     (:foreground ,(mango-theme-color 'khaki '16)))))
 `(git-commit-overlong-summary
   ((((type graphic tty) (min-colors 16777216))
     (:foreground ,(mango-theme-color 'foreground 'gui)
      :background ,(mango-theme-color 'red 'gui) :weight bold))
    (((type tty) (min-colors 256))
     (:foreground ,(mango-theme-color 'foreground '256)
      :background ,(mango-theme-color 'red '256) :weight bold))
    (((type tty))
     (:foreground ,(mango-theme-color 'foreground '16)
      :background ,(mango-theme-color 'red '16) :weight bold))))

 ;; Vertico
 `(vertico-current ((t (:inherit hl-line))))

 ;; Marginalia
 `(mm-diffstat-counter-added ((t (:foreground "green" :weight bold))))
 `(mm-diffstat-counter-removed ((t (:foreground "red" :weight bold))))
 `(marginalia-documentation
   ((((type graphic tty) (min-colors 16777216))
     (:foreground ,(mango-theme-color 'disabled 'gui) :underline nil))
    (((type tty) (min-colors 256))
     (:foreground ,(mango-theme-color 'disabled '256) :underline nil))
    (((type tty))
     (:foreground ,(mango-theme-color 'disabled '16) :underline nil))))

 ;; Tempel
 `(tempel-default
   ((((type graphic tty) (min-colors 16777216))
     (:slant italic :background ,(mango-theme-color 'middleground 'gui)))
    (((type tty) (min-colors 256))
     (:slant italic :background ,(mango-theme-color 'middleground '256)))
    (((type tty))
     (:slant italic :background ,(mango-theme-color 'middleground '16)))))
 `(tempel-field
   ((((type graphic tty) (min-colors 16777216))
     (:slant italic :background ,(mango-theme-color 'middleground 'gui)))
    (((type tty) (min-colors 256))
     (:slant italic :background ,(mango-theme-color 'middleground '256)))
    (((type tty))
     (:slant italic :background ,(mango-theme-color 'middleground '16)))))
 `(tempel-form
   ((((type graphic tty) (min-colors 16777216))
     (:slant italic :background ,(mango-theme-color 'middleground 'gui)))
    (((type tty) (min-colors 256))
     (:slant italic :background ,(mango-theme-color 'middleground '256)))
    (((type tty))
     (:slant italic :background ,(mango-theme-color 'middleground '16))))))

(provide-theme 'mango)
