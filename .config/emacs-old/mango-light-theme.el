;;; mango-light-theme.el --- Just your average light theme  -*- lexical-binding: t; -*-

(deftheme mango-light
  "Mildly light, light theme.
Your average not-so-light light theme, because none of the other options
were exactly to my liking.  It’s about time I had a theme to call my
own.")

;; Colors are defined as: (GUI-HEX 256-COLOR 16-COLOR)
(defconst mango-light-theme-colors-alist
  '(;; (foreground       . ("#3B4252" "color-238" "black"))
    ;; (background       . ("#ECEFF4" "color-255" "white"))
    ;; (background-cool  . ("#E5E9F0" "color-254" "white"))
    ;; (background-dark  . ("#FAFBFC" "color-231" "brightwhite"))
    ;; (background-faint . ("#D8DEE9" "color-253" "brightwhite"))
    ;; (middleground     . ("#C8D0E0" "color-252" "brightwhite"))
    (disabled         . ("#9BA6B5" "color-247" "brightblack"))
    (foreground . ("#000000" "color-0" "black"))
    (background . ("#ffffff" "color-0" "black"))
    (middleground . ("#c4c4c4" "color-0" "black"))
    (background-cool . ("#e0e0e0" "color-0" "black"))
    ;; TODO: Rename to foreground-light
    (background-dark . ("#595959" "color-0" "black"))
    (background-faint . ("#e6e6e6" "color-0" "black"))

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

(custom-theme-set-faces
 'mango-light

 ;; Standard Stuff
 `(default
   ((((type graphic tty) (min-colors 16777216))
     (:foreground ,(mango-light-theme-color 'foreground 'gui)
      :background ,(mango-light-theme-color 'background 'gui)))
    (((type tty) (min-colors 256))
     (:foreground ,(mango-light-theme-color 'foreground '256)
      :background ,(mango-light-theme-color 'background '256)))
    (((type tty))
     (:foreground ,(mango-light-theme-color 'foreground '16)
      :background ,(mango-light-theme-color 'background '16)))))
 `(fringe
   ((t (:inherit default))))

 ;; Lines
 `(hl-line
   ((((type graphic tty) (min-colors 16777216))
     (:background ,(mango-light-theme-color 'background-faint 'gui)))
    (((type tty) (min-colors 256))
     (:background ,(mango-light-theme-color 'background-faint '256)))
    (((type tty))
     (:background ,(mango-light-theme-color 'background-faint '16)))))
 `(region
   ((((type graphic tty) (min-colors 16777216))
     (:background ,(mango-light-theme-color 'middleground 'gui)))
    (((type tty) (min-colors 256))
     (:background ,(mango-light-theme-color 'middleground '256)))
    (((type tty))
     (:background ,(mango-light-theme-color 'middleground '16)))))
 `(header-line
   ((((type graphic tty) (min-colors 16777216))
     (:background ,(mango-light-theme-color 'middleground 'gui)))
    (((type tty) (min-colors 256))
     (:background ,(mango-light-theme-color 'middleground '256)))
    (((type tty))
     (:background ,(mango-light-theme-color 'middleground '16)))))
 `(mode-line-active
   ((t ( :box ,(mango-light-theme-color 'celestial-blue 'gui)
         :inherit header-line))))
 `(mode-line-inactive
   ((((type graphic tty) (min-colors 16777216))
     ( :background ,(mango-light-theme-color 'background-cool 'gui)
       :box ,(mango-light-theme-color 'background-faint 'gui)
       :weight light))
    (((type tty) (min-colors 256))
     ( :background ,(mango-light-theme-color 'background-cool '256)
       :box ,(mango-light-theme-color 'background-faint 'gui)
       :weight light))
    (((type tty))
     ( :background ,(mango-light-theme-color 'background-cool '16)
       :box ,(mango-light-theme-color 'background-faint 'gui)
       :weight light))))
 `(window-divider
   ((((type graphic tty) (min-colors 16777216))
     (:foreground ,(mango-light-theme-color 'background-cool 'gui)))
    (((type tty) (min-colors 256))
     (:foreground ,(mango-light-theme-color 'background-cool '256)))
    (((type tty))
     (:foreground ,(mango-light-theme-color 'background-cool '16)))))
 `(window-divider-first-pixel
   ((((type graphic tty) (min-colors 16777216))
     (:foreground ,(mango-light-theme-color 'background-cool 'gui)))
    (((type tty) (min-colors 256))
     (:foreground ,(mango-light-theme-color 'background-cool '256)))
    (((type tty))
     (:foreground ,(mango-light-theme-color 'background-cool '16)))))
 `(window-divider-last-pixel
   ((((type graphic tty) (min-colors 16777216))
     (:foreground ,(mango-light-theme-color 'background-cool 'gui)))
    (((type tty) (min-colors 256))
     (:foreground ,(mango-light-theme-color 'background-cool '256)))
    (((type tty))
     (:foreground ,(mango-light-theme-color 'background-cool '16)))))

 ;; Line Numbers
 `(line-number
   ((((type graphic tty) (min-colors 16777216))
     (:foreground ,(mango-light-theme-color 'disabled 'gui)
      :background ,(mango-light-theme-color 'background 'gui)))
    (((type tty) (min-colors 256))
     (:foreground ,(mango-light-theme-color 'disabled '256)
      :background ,(mango-light-theme-color 'background '256)))
    (((type tty))
     (:foreground ,(mango-light-theme-color 'disabled '16)
      :background ,(mango-light-theme-color 'background '16)))))
 `(line-number-current-line
   ((((type graphic tty) (min-colors 16777216))
     (:foreground ,(mango-light-theme-color 'salmon 'gui)
      :background ,(mango-light-theme-color 'background 'gui) :weight bold))
    (((type tty) (min-colors 256))
     (:foreground ,(mango-light-theme-color 'salmon '256)
      :background ,(mango-light-theme-color 'background '256) :weight bold))
    (((type tty))
     (:foreground ,(mango-light-theme-color 'salmon '16)
      :background ,(mango-light-theme-color 'background '16) :weight bold))))

 ;; Documentation
 `(font-lock-comment-face
   ((((type graphic tty) (min-colors 16777216))
     (:foreground ,(mango-light-theme-color 'khaki 'gui) :weight semi-bold))
    (((type tty) (min-colors 256))
     (:foreground ,(mango-light-theme-color 'khaki '256) :weight semi-bold))
    (((type tty))
     (:foreground ,(mango-light-theme-color 'khaki '16) :weight semi-bold))))
 `(font-lock-doc-face
   ((((type graphic tty) (min-colors 16777216))
     (:foreground ,(mango-light-theme-color 'disabled 'gui)))
    (((type tty) (min-colors 256))
     (:foreground ,(mango-light-theme-color 'disabled '256)))
    (((type tty))
     (:foreground ,(mango-light-theme-color 'disabled '16)))))

 ;; Modeline
 `(mm-modeline-overwrite-face ((t (:weight normal))))
 `(mm-modeline-readonly-face ((t (:weight normal))))
 `(mm-modeline-buffer-name-face ((t (:inherit font-lock-constant-face))))
 `(mm-modeline-buffer-modified-face ((t (:inherit shadow))))
 `(mm-modeline-major-mode-name-face ((t (:weight normal))))
 `(mm-modeline-major-mode-symbol-face ((t (:inherit shadow))))
 `(mm-modeline-git-branch-face ((t (:inherit font-lock-constant-face))))
 `(mm-modeline-narrow-face
   ((((type graphic tty) (min-colors 16777216))
     (:background ,(mango-light-theme-color 'dark-red 'gui) :foreground ,(mango-light-theme-color 'background-dark 'gui) :box ,(mango-light-theme-color 'dark-red 'gui) :weight normal))
    (((type tty) (min-colors 256))
     (:background ,(mango-light-theme-color 'dark-red '256) :foreground ,(mango-light-theme-color 'background-dark '256) :box ,(mango-light-theme-color 'dark-red '256) :weight normal))
    (((type tty))
     (:background ,(mango-light-theme-color 'dark-red '16) :foreground ,(mango-light-theme-color 'background-dark '16) :box ,(mango-light-theme-color 'dark-red '16) :weight normal))))

 ;; Core Language
 `(font-lock-builtin-face ((t (:inherit font-lock-preprocessor-face))))
 `(font-lock-keyword-face
   ((((type graphic tty) (min-colors 16777216))
     (:foreground ,(mango-light-theme-color 'violet 'gui)))
    (((type tty) (min-colors 256))
     (:foreground ,(mango-light-theme-color 'violet '256)))
    (((type tty))
     (:foreground ,(mango-light-theme-color 'violet '16)))))
 `(font-lock-type-face
   ((((type graphic tty) (min-colors 16777216))
     (:foreground ,(mango-light-theme-color 'celestial-blue 'gui)))
    (((type tty) (min-colors 256))
     (:foreground ,(mango-light-theme-color 'celestial-blue '256)))
    (((type tty))
     (:foreground ,(mango-light-theme-color 'celestial-blue '16)))))

 ;; Function-likes
 `(font-lock-function-name-face
   ((((type graphic tty) (min-colors 16777216))
     (:foreground ,(mango-light-theme-color 'khaki 'gui)))
    (((type tty) (min-colors 256))
     (:foreground ,(mango-light-theme-color 'khaki '256)))
    (((type tty))
     (:foreground ,(mango-light-theme-color 'khaki '16)))))
 `(font-lock-preprocessor-face
   ((((type graphic tty) (min-colors 16777216))
     (:foreground ,(mango-light-theme-color 'magenta 'gui) :weight bold))
    (((type tty) (min-colors 256))
     (:foreground ,(mango-light-theme-color 'magenta '256) :weight bold))
    (((type tty))
     (:foreground ,(mango-light-theme-color 'magenta '16) :weight bold))))

 ;; Variables
 `(font-lock-constant-face ((t (:inherit font-lock-variable-name-face :weight bold))))
 `(font-lock-variable-name-face
   ((((type graphic tty) (min-colors 16777216))
     (:foreground ,(mango-light-theme-color 'pale-azure 'gui)))
    (((type tty) (min-colors 256))
     (:foreground ,(mango-light-theme-color 'pale-azure '256)))
    (((type tty))
     (:foreground ,(mango-light-theme-color 'pale-azure '16)))))

 ;; Other literals
 `(help-key-binding ((t (:inherit font-lock-constant-face))))
 `(font-lock-string-face
   ((((type graphic tty) (min-colors 16777216))
     (:foreground ,(mango-light-theme-color 'salmon 'gui)))
    (((type tty) (min-colors 256))
     (:foreground ,(mango-light-theme-color 'salmon '256)))
    (((type tty))
     (:foreground ,(mango-light-theme-color 'salmon '16)))))
 `(font-lock-number-face
   ((((type graphic tty) (min-colors 16777216))
     (:foreground ,(mango-light-theme-color 'salmon 'gui)))
    (((type tty) (min-colors 256))
     (:foreground ,(mango-light-theme-color 'salmon '256)))
    (((type tty))
     (:foreground ,(mango-light-theme-color 'salmon '16)))))

 ;; Org Mode
 `(org-quote ((t (:inherit org-block :slant italic))))
 `(org-code
   ((((type graphic tty) (min-colors 16777216))
     (:foreground ,(mango-light-theme-color 'salmon 'gui)))
    (((type tty) (min-colors 256))
     (:foreground ,(mango-light-theme-color 'salmon '256)))
    (((type tty))
     (:foreground ,(mango-light-theme-color 'salmon '16)))))
 `(org-verbatim
   ((((type graphic tty) (min-colors 16777216))
     (:foreground ,(mango-light-theme-color 'lime 'gui)))
    (((type tty) (min-colors 256))
     (:foreground ,(mango-light-theme-color 'lime '256)))
    (((type tty))
     (:foreground ,(mango-light-theme-color 'lime '16)))))
 `(org-block
   ((((type graphic tty) (min-colors 16777216))
     (:background ,(mango-light-theme-color 'background-cool 'gui)))
    (((type tty) (min-colors 256))
     (:background ,(mango-light-theme-color 'background-cool '256)))
    (((type tty))
     (:background ,(mango-light-theme-color 'background-cool '16)))))
 `(org-hide
   ((((type graphic tty) (min-colors 16777216))
     (:foreground ,(mango-light-theme-color 'background 'gui)))
    (((type tty) (min-colors 256))
     (:foreground ,(mango-light-theme-color 'background '256)))
    (((type tty))
     (:foreground ,(mango-light-theme-color 'background '16)))))

 ;; Info Page
 `(Info-quoted ((t (:inherit default))))

 ;; Magit
 `(magit-diff-context-highlight ((t (:inherit hl-line))))
 `(magit-section-highlight ((t (:inherit hl-line))))
 `(magit-diff-hunk-heading
   ((((type graphic tty) (min-colors 16777216))
     (:background ,(mango-light-theme-color 'background-cool 'gui)))
    (((type tty) (min-colors 256))
     (:background ,(mango-light-theme-color 'background-cool '256)))
    (((type tty))
     (:background ,(mango-light-theme-color 'background-cool '16)))))
 `(magit-diff-hunk-heading-highlight
   ((((type graphic tty) (min-colors 16777216))
     (:background ,(mango-light-theme-color 'middleground 'gui)))
    (((type tty) (min-colors 256))
     (:background ,(mango-light-theme-color 'middleground '256)))
    (((type tty))
     (:background ,(mango-light-theme-color 'middleground '16)))))
 `(git-commit-summary
   ((((type graphic tty) (min-colors 16777216))
     (:foreground ,(mango-light-theme-color 'khaki 'gui)))
    (((type tty) (min-colors 256))
     (:foreground ,(mango-light-theme-color 'khaki '256)))
    (((type tty))
     (:foreground ,(mango-light-theme-color 'khaki '16)))))
 `(git-commit-overlong-summary
   ((((type graphic tty) (min-colors 16777216))
     (:foreground ,(mango-light-theme-color 'background-dark 'gui)
      :background ,(mango-light-theme-color 'red 'gui) :weight bold))
    (((type tty) (min-colors 256))
     (:foreground ,(mango-light-theme-color 'background-dark '256)
      :background ,(mango-light-theme-color 'red '256) :weight bold))
    (((type tty))
     (:foreground ,(mango-light-theme-color 'background-dark '16)
      :background ,(mango-light-theme-color 'red '16) :weight bold))))

 ;; Vertico
 `(vertico-current ((t (:inherit hl-line))))

 ;; Marginalia
 `(mm-diffstat-counter-added ((t (:foreground "green" :weight bold))))
 `(mm-diffstat-counter-removed ((t (:foreground "red" :weight bold))))
 `(marginalia-documentation
   ((((type graphic tty) (min-colors 16777216))
     (:foreground ,(mango-light-theme-color 'disabled 'gui) :underline nil))
    (((type tty) (min-colors 256))
     (:foreground ,(mango-light-theme-color 'disabled '256) :underline nil))
    (((type tty))
     (:foreground ,(mango-light-theme-color 'disabled '16) :underline nil))))

 ;; Tempel
 `(tempel-default
   ((((type graphic tty) (min-colors 16777216))
     (:slant italic :background ,(mango-light-theme-color 'middleground 'gui)))
    (((type tty) (min-colors 256))
     (:slant italic :background ,(mango-light-theme-color 'middleground '256)))
    (((type tty))
     (:slant italic :background ,(mango-light-theme-color 'middleground '16)))))
 `(tempel-field
   ((((type graphic tty) (min-colors 16777216))
     (:slant italic :background ,(mango-light-theme-color 'middleground 'gui)))
    (((type tty) (min-colors 256))
     (:slant italic :background ,(mango-light-theme-color 'middleground '256)))
    (((type tty))
     (:slant italic :background ,(mango-light-theme-color 'middleground '16)))))
 `(tempel-form
   ((((type graphic tty) (min-colors 16777216))
     (:slant italic :background ,(mango-light-theme-color 'middleground 'gui)))
    (((type tty) (min-colors 256))
     (:slant italic :background ,(mango-light-theme-color 'middleground '256)))
    (((type tty))
     (:slant italic :background ,(mango-light-theme-color 'middleground '16))))))

(provide-theme 'mango-light)
