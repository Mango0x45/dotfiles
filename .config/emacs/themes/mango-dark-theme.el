;;; mango-dark-theme.el --- Just your average dark theme  -*- lexical-binding: t; -*-

(deftheme mango-dark
  "Just your average dark theme.")

(defconst mango-dark-theme-colors-alist
  '((fg-dim         . ("#939CA8" "color-246" "brightblack"))
    (fg-main        . ("#D1D5D8" "color-251" "white"))
    (fg-bright      . ("#F0F4F8" "color-255" "brightwhite"))
    (bg-dim         . ("#1D232F" "color-234" "black"))
    (bg-main        . ("#2B303B" "color-236" "black"))
    (bg-bright      . ("#414859" "color-238" "brightblack"))
    (red-dim        . ("#A42A22" "color-88"  "red"))
    (red-main       . ("#F24E4E" "color-160" "brightred"))
    (red-bright     . ("#FF7373" "color-203" "brightred"))
    (orange-dim     . ("#A66B40" "color-130" "yellow"))
    (orange-main    . ("#ECA671" "color-216" "brightyellow"))
    (orange-bright  . ("#FFC299" "color-223" "brightyellow"))
    (yellow-dim     . ("#B8A349" "color-143" "yellow"))
    (yellow-main    . ("#E5D070" "color-228" "yellow"))
    (yellow-bright  . ("#FFEE99" "color-229" "brightyellow"))
    (green-dim      . ("#7BAF20" "color-106" "green"))
    (green-main     . ("#A6E22E" "color-156" "green"))
    (green-bright   . ("#C4F553" "color-191" "brightgreen"))
    (cyan-dim       . ("#569BBB" "color-73"  "cyan"))
    (cyan-main      . ("#7DC1E6" "color-117" "cyan"))
    (cyan-bright    . ("#A8E0FF" "color-153" "brightcyan"))
    (blue-dim       . ("#427EB3" "color-31"  "blue"))
    (blue-main      . ("#569CD6" "color-74"  "brightblue"))
    (blue-bright    . ("#85C5FF" "color-117" "brightblue"))
    (magenta-dim    . ("#A151A6" "color-133" "magenta"))
    (magenta-main   . ("#E183E8" "color-213" "magenta"))
    (magenta-bright . ("#FFACFF" "color-219" "brightmagenta"))
    (violet-dim     . ("#8A4A9E" "color-97"  "magenta"))
    (violet-main    . ("#C678DD" "color-176" "brightmagenta"))
    (violet-bright  . ("#E8A3FF" "color-219" "brightmagenta")))
  "The color palette used throughout `mango-dark-theme'.")

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
     :foreground 'red-main
     :weight 'bold))
 `(mm-modeline-read-only
   ,(mango-dark-theme-spec
     :foreground 'yellow-main
     :weight 'semi-bold))
 `(mm-modeline-narrowed
   ,(mango-dark-theme-spec
     :foreground 'green-main
     :weight 'bold))
 `(mm-modeline-overwrite
   ,(mango-dark-theme-spec
     :foreground 'magenta-main
     :weight 'bold))
 `(mm-modeline-region
   ,(mango-dark-theme-spec
     :background 'bg-bright
     :weight 'bold))
 `(mm-modeline-position
   ,(mango-dark-theme-spec
     :foreground 'cyan-main
     :weight 'bold))
 `(mm-modeline-major-mode
   ,(mango-dark-theme-spec
     :foreground 'violet-main
     :weight 'bold))
 `(mm-modeline-vc
   ,(mango-dark-theme-spec
     :foreground 'green-main
     :weight 'bold))

 ;; Tab Bar
 `(tab-bar
   ,(mango-dark-theme-spec
     :background 'bg-dim
     :foreground 'fg-main))
 `(tab-bar-tab
   ,(mango-dark-theme-spec
     :background 'bg-main
     :foreground 'fg-main
     :weight 'bold
     :underline `( :color ,(mango-dark-theme-color 'blue-main 'gui)
                   :style line
                   :position -1)))
 `(tab-bar-tab-inactive
   ,(mango-dark-theme-spec
     :background 'bg-dim
     :foreground 'fg-dim))

 ;; Lines
 `(hl-line
   ,(mango-dark-theme-spec
     :background 'bg-bright))
 `(region
   ,(mango-dark-theme-spec
     :background 'bg-bright))
 `(header-line
   ,(mango-dark-theme-spec
     :background 'bg-bright))
 `(mode-line-active
   ((t ( :box ,(mango-dark-theme-color 'fg-main 'gui)
         :inherit header-line))))
 `(mode-line-inactive
   ,(mango-dark-theme-spec
     :background 'bg-dim
     :box 'bg-bright
     :weight 'light))
 `(icomplete-selected-match
   ,(mango-dark-theme-spec
     :background 'bg-bright))
 `(window-divider
   ,(mango-dark-theme-spec
     :foreground 'bg-dim))
 `(window-divider-first-pixel
   ,(mango-dark-theme-spec
     :foreground 'bg-dim))
 `(window-divider-last-pixel
   ,(mango-dark-theme-spec
     :foreground 'bg-dim))

 ;; Line Numbers
 `(line-number
   ,(mango-dark-theme-spec
     :foreground 'fg-dim
     :background 'bg-main))
 `(line-number-current-line
   ,(mango-dark-theme-spec
     :foreground 'orange-main
     :background 'bg-main
     :weight 'bold))

 ;; Documentation
 `(font-lock-comment-face
   ,(mango-dark-theme-spec
     :foreground 'yellow-main
     :weight 'semi-bold))
 `(font-lock-doc-face
   ,(mango-dark-theme-spec
     :foreground 'fg-dim))

 ;; Core Language
 `(font-lock-builtin-face
   ((t (:inherit font-lock-preprocessor-face))))
 `(font-lock-keyword-face
   ,(mango-dark-theme-spec
     :foreground 'violet-main))
 `(font-lock-type-face
   ,(mango-dark-theme-spec
     :foreground 'blue-main))

 ;; Function-likes
 `(font-lock-function-name-face
   ,(mango-dark-theme-spec
     :foreground 'yellow-main))
 `(font-lock-preprocessor-face
   ,(mango-dark-theme-spec
     :foreground 'magenta-bright
     :weight 'bold))

 ;; Variables
 `(font-lock-constant-face
   ((t ( :inherit font-lock-variable-name-face
         :weight bold))))
 `(font-lock-variable-name-face
   ,(mango-dark-theme-spec
     :foreground 'cyan-main))

 ;; Other literals
 `(help-key-binding
   ((t (:inherit font-lock-constant-face))))
 `(font-lock-number-face
   ,(mango-dark-theme-spec
     :foreground 'orange-main))

 ;; Org Mode
 `(org-quote
   ((t ( :inherit org-block
         :slant italic))))
 `(org-code
   ,(mango-dark-theme-spec
     :foreground 'orange-main))
 `(org-verbatim
   ,(mango-dark-theme-spec
     :foreground 'green-main))
 `(org-block
   ,(mango-dark-theme-spec
     :background 'bg-dim))
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
     :background 'bg-dim))
 `(magit-diff-hunk-heading-highlight
   ,(mango-dark-theme-spec
     :background 'bg-bright))
 `(git-commit-summary
   ,(mango-dark-theme-spec
     :foreground 'yellow-main))
 `(git-commit-overlong-summary
   ,(mango-dark-theme-spec
     :foreground 'fg-main
     :background 'red-main
     :weight 'bold))

 ;; Vertico
 `(vertico-current ((t (:inherit hl-line))))

 ;; Marginalia
 `(mm-diffstat-counter-added
   ,(mango-dark-theme-spec
     :foreground 'green-main
     :weight 'bold))
 `(mm-diffstat-counter-removed
   ,(mango-dark-theme-spec
     :foreground 'red-main
     :weight 'bold))
 `(marginalia-documentation
   ,(mango-dark-theme-spec
     :foreground 'fg-dim
     :underline nil))

 ;; Tempel
 `(tempel-default
   ,(mango-dark-theme-spec
     :slant 'italic
     :background 'bg-bright))
 `(tempel-field
   ,(mango-dark-theme-spec
     :slant 'italic
     :background 'bg-bright))
 `(tempel-form
   ,(mango-dark-theme-spec
     :slant 'italic
     :background 'bg-bright))

 ;; Hl-Todo
 `(mm-theme-hl-todo-fixme
   ,(mango-dark-theme-spec
     :foreground 'red-main
     :inherit 'hl-todo))
 `(mm-theme-hl-todo-note
   ,(mango-dark-theme-spec
     :foreground 'cyan-main
     :inherit 'hl-todo))
 `(mm-theme-hl-todo-todo
   ,(mango-dark-theme-spec
     :foreground 'magenta-main
     :inherit 'hl-todo)))

(provide-theme 'mango-dark)
