;;; mm-keybindings.el --- Emacs keybindings  -*- lexical-binding: t; -*-

(require 'editing-functions)

;; The following keys are either unbound and are free to populate, or are
;; bound to functions I don’t care for:
;; ‘C-i’, ‘C-j’, ‘C-o’, ‘C-{’, ‘C-}’, ‘C-/’, ‘C-\;’, ‘C-:’


;;; Helper Macros

(defmacro mm-keybindings-keymap-set (keymap &rest definitions)
  "TODO"
  (declare (indent 1))
  (unless (cl-evenp (length definitions))
    (user-error "Expected an even-number of elements in DEFINITIONS."))
  `(cl-loop for (from to) on (list ,@definitions) by #'cddr
            do (keymap-set ,keymap from to)))

(defmacro mm-keybindings-keymap-set-repeating (keymap &rest definitions)
  "TODO"
  (declare (indent 1))
  (unless (cl-evenp (length definitions))
    (user-error "Expected an even-number of elements in DEFINITIONS."))
  (let ((keymap-gen (gensym "mm-keybindings--repeat-map-")))
    `(progn
       (defvar-keymap ,keymap-gen)
       (cl-loop for (from to) on (list ,@definitions) by #'cddr
                do (progn
                     (keymap-set ,keymap-gen from to)
                     (put to 'repeat-map ',keymap-gen))))))

(defmacro mm-keybindings-keymap-remap (keymap &rest commands)
  "Define command remappings for a given KEYMAP.
COMMANDS is a sequence of unquoted commands.  For each pair of
COMMANDS the first command is remapped to the second command."
  (declare (indent 1))
  (unless (cl-evenp (length commands))
    (user-error "Expected an even-number of elements in COMMANDS."))
  (macroexp-progn
   (cl-loop for (from to) in (seq-partition commands 2)
            collect `(keymap-set
                      ,keymap
                      ,(concat "<remap> <" (symbol-name from) ">")
                      #',to))))


;;; Support the Kitty Keyboard Protocol

;; PKG-EXTERN
(use-package kkp
  :ensure t
  :unless (or (display-graphic-p) mm-humanwave-p)
  :hook (tty-setup . global-kkp-mode))


;;; Support QMK Hyper

(defun mm-keybindings-qmk-hyper-as-hyper (args)
  "Around advice for `keymap-set' to handle QMK hyper."
  (let ((chord (cadr args)))
    (when (string-prefix-p "H-" chord)
      (setf (cadr args) (concat "C-M-S-s" (substring chord 1)))))
  args)

;; Both ‘keymap-global-set’ and ‘keymap-local-set’ call ‘keymap-set’
;; internally, so this advice covers all cases
(advice-add #'keymap-set :filter-args #'mm-keybindings-qmk-hyper-as-hyper)


;;; Disable ESC as Meta

(keymap-global-set "<escape>" #'ignore)


;;; Enable Repeat Bindings

(defun mm-keybindings-enable-repeat-mode ()
  "Enable `repeat-mode' without polluting the echo area."
  (mm-with-suppressed-output
    (repeat-mode)))

(use-package repeat
  :hook (after-init . mm-keybindings-enable-repeat-mode)
  :custom
  (repeat-exit-timeout 5))


;;; Remap Existing Bindings

(mm-keybindings-keymap-remap global-map
  backward-delete-char-untabify backward-delete-char

  capitalize-word capitalize-dwim
  downcase-word   downcase-dwim
  upcase-word     upcase-dwim

  delete-indentation ef-join-current-and-next-line
  mark-sexp          ef-mark-entire-sexp
  mark-word          ef-mark-entire-word
  open-line          ef-open-line
  yank               ef-yank)

(with-eval-after-load 'cc-vars
  (setopt c-backspace-function #'backward-delete-char))


;;; Remove Unwanted Bindings

(keymap-global-unset "C-x C-c" :remove) ; ‘capitalize-region’
(keymap-global-unset "C-x C-l" :remove) ; ‘downcase-region’
(keymap-global-unset "C-x C-u" :remove) ; ‘upcase-region’

;; The following conflicts with ‘ace-window’
(use-package mhtml-mode
  :after ace-window
  :config
  (keymap-unset html-mode-map "M-o" :remove))


;;; Bind Commands Globally

(mm-keybindings-keymap-set global-map
  "<next>"    #'forward-page
  "<prior>"   #'backward-page
  "C-<next>"  #'scroll-up
  "C-<prior>" #'scroll-down

  "C-." #'repeat
  "C-^" #'ef-split-line
  "C-/" #'ef-mark-line-dwim
  "C-]" #'ef-search-forward-char

  "M-\\" #'cycle-spacing

  "C-c c a" #'mc/vertical-align-with-space
  "C-c c i" #'mc/insert-numbers
  "C-c c t" #'mce-transpose-cursor-regions
  "C-c c s" #'ef-sort-dwim
  "C-c c f" #'fill-paragraph
  "C-c d"   #'duplicate-dwim)

(mm-keybindings-keymap-set-repeating global-map
  "j" #'ef-join-current-and-next-line
  "J" #'join-line)

(mm-keybindings-keymap-set-repeating global-map
  "n" #'next-error
  "p" #'previous-error)

(with-eval-after-load 'increment
  (mm-keybindings-keymap-set-repeating global-map
    "C-x" #'decrement-number-at-point
    "C-a" #'increment-number-at-point))


;;; Other Bindings

(with-eval-after-load 'project
  (with-eval-after-load 'grab
    (mm-keybindings-keymap-set project-prefix-map
      "G" #'project-git-grab))

  (when mm-humanwave-p
    (mm-keybindings-keymap-set project-prefix-map
      "q" #'mm-humanwave-query)))

(use-package minibuffer
  :if mm-humanwave-p
  :config
  (mm-keybindings-keymap-set minibuffer-mode-map
    "C-c m" #'mm-humanwave-insert-last-commit-message))


;;; Display Available Keybindings

;; PKG-EXTERN
(use-package which-key
  :hook after-init
  :custom
  (which-key-dont-use-unicode nil)
  (which-key-ellipsis "…")
  (wihch-key-idle-delay .5))

(provide 'mm-keybindings)
