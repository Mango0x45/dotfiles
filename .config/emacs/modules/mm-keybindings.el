;;; mm-keybindings.el --- Emacs keybindings  -*- lexical-binding: t; -*-

(require 'editing)

;; The following keys are either unbound and are free to populate, or are
;; bound to functions I don’t care for:
;; ‘C-i’, ‘C-j’, ‘C-o’, ‘C-{’, ‘C-}’, ‘C-|’, ‘C-/’, ‘C-\;’, ‘C-:’


;;; Helper Macros

(defmacro mm-keymap-set (keymap &rest definitions)
  (declare (indent 1))
  (unless (cl-evenp (length definitions))
    (user-error "Expected an even-number of elements in DEFINITIONS."))
  `(cl-loop for (from to) on (list ,@definitions) by #'cddr
            do (keymap-set ,keymap from to)))

(defmacro mm-keymap-set-repeating (keymap &rest definitions)
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

(defmacro mm-keymap-remap (keymap &rest commands)
  "Define command remappings for a given KEYMAP.
COMMANDS is a sequence of unquoted commands.  For each pair of COMMANDS
the first command is remapped to the second command."
  (declare (indent 1))
  (unless (cl-evenp (length commands))
    (user-error "Expected an even-number of elements in COMMANDS."))
  (macroexp-progn
   (cl-loop for (from to) in (seq-partition commands 2)
            collect `(keymap-set
                      ,keymap
                      ,(concat "<remap> <" (symbol-name from) ">")
                      #',to))))


;;; Support QMK Hyper

(defun mm-qmk-hyper-as-hyper (args)
  (let ((chord (cadr args)))
    (when (string-prefix-p "H-" chord)
      (setf (cadr args) (concat "C-M-S-s" (substring chord 1)))))
  args)

;; Both ‘keymap-global-set’ and ‘keymap-local-set’ call ‘keymap-set’
;; internally, so this advice covers all cases
(advice-add #'keymap-set :filter-args #'mm-qmk-hyper-as-hyper)


;;; Disable ESC as Meta

(keymap-global-set "<escape>" #'ignore)


;;; Enable Repeat Bindings

(defun mm-enable-repeat-mode ()
  "Enable `repeat-mode' without polluting the echo area."
  (mm-with-suppressed-output
    (repeat-mode)))

(use-package repeat
  :hook (after-init . mm-enable-repeat-mode)
  :custom
  (repeat-exit-timeout 5))


;;; Remap Existing Bindings

(mm-keymap-remap global-map
  backward-delete-char-untabify backward-delete-char

  capitalize-word capitalize-dwim
  downcase-word   downcase-dwim
  upcase-word     upcase-dwim

  delete-indentation e/join-current-and-next-line
  kill-ring-save     e/kill-ring-save-dwim
  mark-sexp          e/mark-entire-sexp
  mark-word          e/mark-entire-word
  open-line          e/open-line
  yank               e/yank)

(with-eval-after-load 'cc-vars
  (setopt c-backspace-function #'backward-delete-char))


;;; Remove Unwanted Bindings

(keymap-global-unset "C-x C-c" :remove) ; ‘capitalize-region’
(keymap-global-unset "C-x C-l" :remove) ; ‘downcase-region’
(keymap-global-unset "C-x C-u" :remove) ; ‘upcase-region’


;;; Bind Commands Globally

(mm-keymap-set global-map
  ;; "<next>"    #'e/scroll-up
  ;; "<prior>"   #'e/scroll-down
  "C-<next>"  #'forward-page
  "C-<prior>" #'backward-page

  "C-." #'repeat
  "C-^" #'e/split-line
  "C-/" #'e/mark-line-dwim

  "C-]" #'mm-search-forward-char
  "M-]" #'mm-search-backward-char

  "M-\\" #'cycle-spacing

  "C-M-@" #'mm-add-cursor-to-next-word

  "C-c c t" #'mm-transpose-cursor-regions
  "C-c d"   #'duplicate-dwim
  "C-c t a" #'e/align-regexp
  "C-c t f" #'fill-paragraph
  "C-c t s" #'e/sort-dwim)

(mm-keymap-set-repeating global-map
  "j" #'e/join-current-and-next-line
  "J" #'join-line)

(mm-keymap-set-repeating global-map
  "n" #'next-error
  "p" #'previous-error)

(with-eval-after-load 'increment
  (mm-keymap-set-repeating global-map
    "d" #'decrement-number-at-point
    "i" #'increment-number-at-point))


;;; Display Available Keybindings

(use-package which-key
  :hook after-init
  :custom
  (which-key-dont-use-unicode nil)
  (which-key-ellipsis "…")
  (wihch-key-idle-delay .5))

(provide 'mm-keybindings)