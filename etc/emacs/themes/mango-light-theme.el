;;; mango-light-theme.el --- Just your average light theme  -*- lexical-binding: t; -*-

(deftheme mango-light
  "Just your average light theme.")

(defconst mango-light-theme-colors-alist
  '((fg-dim         . ("#696C77" "color-243" "brightblack"))
    (fg-main        . ("#383A42" "color-237" "black"))
    (fg-bright      . ("#000000" "color-16"  "black"))
    (bg-dim         . ("#E8EAEF" "color-254" "white"))
    (bg-main        . ("#F4F5F7" "color-255" "brightwhite"))
    (bg-bright      . ("#E1E4EA" "color-252" "white"))
    (red-dim        . ("#A02B2B" "color-124" "red"))
    (red-main       . ("#D03E3E" "color-160" "brightred"))
    (red-bright     . ("#E84A4A" "color-167" "brightred"))
    (orange-dim     . ("#8F4512" "color-94"  "yellow"))
    (orange-main    . ("#B85C19" "color-130" "brightyellow"))
    (orange-bright  . ("#D97325" "color-166" "brightyellow"))
    (yellow-dim     . ("#7A5300" "color-94"  "yellow"))
    (yellow-main    . ("#986800" "color-136" "yellow"))
    (yellow-bright  . ("#B57D00" "color-142" "brightyellow"))
    (green-dim      . ("#2E5C16" "color-22"  "green"))
    (green-main     . ("#40801F" "color-28"  "green"))
    (green-bright   . ("#52A628" "color-70"  "brightgreen"))
    (cyan-dim       . ("#1B546A" "color-23"  "cyan"))
    (cyan-main      . ("#287A99" "color-31"  "cyan"))
    (cyan-bright    . ("#359BBD" "color-38"  "brightcyan"))
    (blue-dim       . ("#204A87" "color-18"  "blue"))
    (blue-main      . ("#2E68B8" "color-25"  "brightblue"))
    (blue-bright    . ("#3E84E5" "color-33"  "brightblue"))
    (magenta-dim    . ("#7A287A" "color-89"  "magenta"))
    (magenta-main   . ("#A838A8" "color-127" "magenta"))
    (magenta-bright . ("#C44DC4" "color-163" "brightmagenta"))
    (violet-dim     . ("#632582" "color-54"  "magenta"))
    (violet-main    . ("#8F38B8" "color-90"  "brightmagenta"))
    (violet-bright  . ("#A94AD9" "color-134" "brightmagenta")))
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
   ((t ( :inherit default))))

 ;; Modeline
 `(mm-modeline-modified
   ,(mango-light-theme-spec
     :foreground 'red-main
     :weight 'bold))
 `(mm-modeline-read-only
   ,(mango-light-theme-spec
     :foreground 'yellow-main
     :weight 'semi-bold))
 `(mm-modeline-narrowed
   ,(mango-light-theme-spec
     :foreground 'green-main
     :weight 'bold))
 `(mm-modeline-overwrite
   ,(mango-light-theme-spec
     :foreground 'magenta-main
     :weight 'bold))
 `(mm-modeline-recording-macro
   ,(mango-light-theme-spec
     :foreground 'red-main
     :weight 'bold))
 `(mm-modeline-region
   ,(mango-light-theme-spec
     :background 'bg-bright
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
     :background 'bg-dim
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
     :background 'bg-bright))
 `(region
   ,(mango-light-theme-spec
     :background 'bg-bright))
 `(header-line
   ,(mango-light-theme-spec
     :background 'bg-bright))
 `(mode-line-active
   ((t ( :box ,(mango-light-theme-color 'fg-main 'gui)
         :inherit header-line))))
 `(mode-line-inactive
   ,(mango-light-theme-spec
     :background 'bg-dim
     :box 'bg-bright
     :weight 'light))
 `(icomplete-selected-match
   ,(mango-light-theme-spec
     :background 'bg-bright))
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
     :foreground 'violet-main
     :weight 'bold))
 `(font-lock-type-face
   ,(mango-light-theme-spec
     :foreground 'blue-main))

 ;; Function-likes
 `(font-lock-function-name-face
   ,(mango-light-theme-spec
     :foreground 'yellow-main))
 `(font-lock-preprocessor-face
   ,(mango-light-theme-spec
     :foreground 'magenta-main
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
 `(font-lock-string-face
   ,(mango-dark-theme-spec
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
     :background 'bg-bright))
 `(git-commit-summary
   ,(mango-light-theme-spec
     :foreground 'yellow-main))
 `(git-commit-overlong-summary
   ,(mango-light-theme-spec
     :foreground 'bg-main
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
     :background 'bg-bright))
 `(tempel-field
   ,(mango-light-theme-spec
     :slant 'italic
     :background 'bg-bright))
 `(tempel-form
   ,(mango-light-theme-spec
     :slant 'italic
     :background 'bg-bright))

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
     :foreground 'magenta-main
     :inherit 'hl-todo))

 ;; Multiple Cursors
 `(mc/cursor-face
   ,(mango-light-theme-spec
     :background 'fg-dim)))

(provide-theme 'mango-light)
