;;; mango-light-theme.el --- Just your average light theme  -*- lexical-binding: t; -*-

(deftheme mango-light
  "Mildly light, light theme.
Your average not-so-light light theme, because none of the other options
were exactly to my liking.  It’s about time I had a theme to call my
own.")

(defconst mango-light-theme-colors-alist
  '((fg-dim         . ("#6B7A8F" "color-243" "brightblack"))
    (fg-main        . ("#2E3440" "color-237" "black"))
    (fg-bright      . ("#181B21" "color-234" "black"))
    (bg-dim         . ("#E5E9F0" "color-254" "white"))
    (bg-main        . ("#FAFBFC" "color-231" "brightwhite"))
    (bg-bright      . ("#FFFFFF" "color-231" "brightwhite"))
    (red-dim        . ("#D44A50" "color-167" "red"))
    (red-main       . ("#B51A22" "color-160" "brightred"))
    (red-bright     . ("#8A0D14" "color-88"  "red"))
    (orange-dim     . ("#D97A3E" "color-173" "yellow"))
    (orange-main    . ("#B84C09" "color-166" "brightyellow"))
    (orange-bright  . ("#8A3300" "color-88"  "red"))
    (yellow-dim     . ("#A68435" "color-137" "yellow"))
    (yellow-main    . ("#735610" "color-94"  "yellow"))
    (yellow-bright  . ("#523B06" "color-58"  "yellow"))
    (green-dim      . ("#42A835" "color-71"  "green"))
    (green-main     . ("#286620" "color-28"  "green"))
    (green-bright   . ("#154210" "color-22"  "green"))
    (cyan-dim       . ("#009EE6" "color-38"  "cyan"))
    (cyan-main      . ("#006B8F" "color-31"  "cyan"))
    (cyan-bright    . ("#004761" "color-23"  "cyan"))
    (blue-dim       . ("#3B80F2" "color-69"  "blue"))
    (blue-main      . ("#1856B8" "color-26"  "blue"))
    (blue-bright    . ("#0D3A82" "color-18"  "blue"))
    (magenta-dim    . ("#AD47C7" "color-134" "magenta"))
    (magenta-main   . ("#8828A1" "color-127" "magenta"))
    (magenta-bright . ("#5C1770" "color-53"  "magenta"))
    (violet-dim     . ("#924BB8" "color-133" "magenta"))
    (violet-main    . ("#6B338A" "color-97"  "magenta"))
    (violet-bright  . ("#4A1E61" "color-54"  "magenta")))
  "The color palette used throughout `mango-light-theme'.")

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
     :foreground 'red-main
     :weight 'bold))
 `(mm-modeline-read-only
   ,(mango-light-theme-spec
     :foreground 'yellow-main
     :weight 'bold))
 `(mm-modeline-narrowed
   ,(mango-light-theme-spec
     :foreground 'blue-main
     :weight 'bold))
 `(mm-modeline-overwrite
   ,(mango-light-theme-spec
     :foreground 'magenta-main
     :weight 'bold))
 `(mm-modeline-region
   ,(mango-light-theme-spec
     :foreground 'fg-main
     :background 'bg-dim
     :weight 'bold))
 `(mm-modeline-position
   ,(mango-light-theme-spec
     :foreground 'cyan-main
     :weight 'bold))
 `(mm-modeline-major-mode
   ,(mango-light-theme-spec
     :foreground 'violet-main
     :weight 'bold))
 `(mm-modeline-vc
   ,(mango-light-theme-spec
     :foreground 'green-main
     :weight 'bold))

 ;; Tab Bar
 `(tab-bar
   ,(mango-light-theme-spec
     :background 'bg-bright
     :foreground 'fg-main))
 `(tab-bar-tab
   ,(mango-light-theme-spec
     :background 'bg-main
     :foreground 'fg-main
     :weight 'bold
     :underline `( :color ,(mango-light-theme-color 'blue-main 'gui)
                   :style line
                   :position -1)))
 `(tab-bar-tab-inactive
   ,(mango-light-theme-spec
     :background 'bg-dim
     :foreground 'fg-dim))

 ;; Lines
 `(hl-line
   ,(mango-light-theme-spec
     :background 'bg-dim))
 `(region
   ,(mango-light-theme-spec
     :background 'bg-dim))
 `(header-line
   ,(mango-light-theme-spec
     :background 'bg-dim))
 `(mode-line-active
   ((t ( :box ,(mango-light-theme-color 'fg-main 'gui)
         :inherit header-line))))
 `(mode-line-inactive
   ,(mango-light-theme-spec
     :background 'bg-bright
     :box 'bg-dim
     :weight 'light))
 `(icomplete-selected-match
   ,(mango-light-theme-spec
     :background 'bg-dim))
 `(window-divider
   ,(mango-light-theme-spec
     :foreground 'bg-bright))
 `(window-divider-first-pixel
   ,(mango-light-theme-spec
     :foreground 'bg-bright))
 `(window-divider-last-pixel
   ,(mango-light-theme-spec
     :foreground 'bg-bright))

 ;; Line Numbers
 `(line-number
   ,(mango-light-theme-spec
     :foreground 'fg-dim
     :background 'bg-main))
 `(line-number-current-line
   ,(mango-light-theme-spec
     :foreground 'orange-main
     :background 'bg-main
     :weight 'bold))

 ;; Documentation
 `(font-lock-comment-face
   ,(mango-light-theme-spec
     :foreground 'yellow-main
     :weight 'semi-bold))
 `(font-lock-doc-face
   ,(mango-light-theme-spec
     :foreground 'fg-dim))

 ;; Core Language
 `(font-lock-builtin-face
   ((t (:inherit font-lock-preprocessor-face))))
 `(font-lock-keyword-face
   ,(mango-light-theme-spec
     :foreground 'violet-main))
 `(font-lock-type-face
   ,(mango-light-theme-spec
     :foreground 'blue-main))

 ;; Function-likes
 `(font-lock-function-name-face
   ,(mango-light-theme-spec
     :foreground 'yellow-main))
 `(font-lock-preprocessor-face
   ,(mango-light-theme-spec
     :foreground 'magenta-bright
     :weight 'bold))

 ;; Variables
 `(font-lock-constant-face
   ((t ( :inherit font-lock-variable-name-face
         :weight bold))))
 `(font-lock-variable-name-face
   ,(mango-light-theme-spec
     :foreground 'cyan-main))

 ;; Other literals
 `(help-key-binding
   ((t (:inherit font-lock-constant-face))))
 `(font-lock-number-face
   ,(mango-light-theme-spec
     :foreground 'orange-main))

 ;; Org Mode
 `(org-quote
   ((t ( :inherit org-block
         :slant italic))))
 `(org-code
   ,(mango-light-theme-spec
     :foreground 'orange-main))
 `(org-verbatim
   ,(mango-light-theme-spec
     :foreground 'green-main))
 `(org-block
   ,(mango-light-theme-spec
     :background 'bg-bright))
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
     :background 'bg-bright))
 `(magit-diff-hunk-heading-highlight
   ,(mango-light-theme-spec
     :background 'bg-dim))
 `(git-commit-summary
   ,(mango-light-theme-spec
     :foreground 'yellow-main))
 `(git-commit-overlong-summary
   ,(mango-light-theme-spec
     :foreground 'fg-bright
     :background 'red-main
     :weight 'bold))

 ;; Vertico
 `(vertico-current ((t (:inherit hl-line))))

 ;; Marginalia
 `(mm-diffstat-counter-added
   ,(mango-light-theme-spec
     :foreground 'green-main
     :weight 'bold))
 `(mm-diffstat-counter-removed
   ,(mango-light-theme-spec
     :foreground 'red-main
     :weight 'bold))
 `(marginalia-documentation
   ,(mango-light-theme-spec
     :foreground 'fg-dim
     :underline nil))

 ;; Tempel
 `(tempel-default
   ,(mango-light-theme-spec
     :slant 'italic
     :background 'bg-dim))
 `(tempel-field
   ,(mango-light-theme-spec
     :slant 'italic
     :background 'bg-dim))
 `(tempel-form
   ,(mango-light-theme-spec
     :slant 'italic
     :background 'bg-dim))

 ;; Hl-Todo
 `(mm-theme-hl-todo-fixme
   ,(mango-light-theme-spec
     :foreground 'red-main
     :inherit 'hl-todo))
 `(mm-theme-hl-todo-note
   ,(mango-light-theme-spec
     :foreground 'cyan-main
     :inherit 'hl-todo))
 `(mm-theme-hl-todo-todo
   ,(mango-light-theme-spec
     :foreground 'violet-main
     :inherit 'hl-todo)))

(provide-theme 'mango-light)
