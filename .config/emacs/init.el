;;; init.el --- Emacs configuration file  -*- lexical-binding: t; -*-

;;; Preamble
(x-set user-full-name    "Thomas Voss"
       user-mail-address "mail@thomasvoss.com")

(when (< emacs-major-version 29)
  (error "Emacs 29 or newer is required"))

(when (native-comp-available-p)
  (x-set
    native-comp-async-report-warnings-errors nil
    native-comp-verbose 0
    native-comp-debug 0
    native-comp-jit-compilation t
    native-compile-prune-cache t))

(require 'package)
(x-set
  package-archives (let ((protocol (if (gnutls-available-p) "https" "http")))
                     (mapcar
                      (lambda (pair)
                        (cons (car pair) (concat protocol "://" (cdr pair))))
                      '(("gnu"    . "elpa.gnu.org/packages/")
                        ("nongnu" . "elpa.nongnu.org/nongnu/")
                        ("melpa"  . "melpa.org/packages/"))))
  package-archive-priorities '(("gnu"    . 3)
                               ("nongnu" . 2)
                               ("melpa"  . 1))
  package-user-dir (expand-file-name "pkg" x-data-directory))

(package-initialize)

(eval-and-compile
  (x-set
    use-package-always-defer t
    use-package-always-ensure t
    use-package-expand-minimally t))

(eval-when-compile
  (require 'use-package))

;;; Convenience Macros and -Functions
(defmacro λ (&rest body)
  "Convenience macro to create lambda functions that take no arguments
with much shorter and concise syntax.  Calling ‘λ’ with BODY is
equivalent to calling ‘lambda’ with an empty argument list and BODY."
  (declare (pure t) (side-effect-free t))
  `(lambda () ,@body))

(defmacro λi (&rest body)
  "Convenience macro to create interactive lambda functions that take
no arguments with much shorter and concise syntax.  Calling ‘λi’ with
BODY is equivalent to calling ‘lambda’ with an empty argument list and
BODY directly after ‘(interactive)’."
  (declare (pure t) (side-effect-free t))
  `(lambda () (interactive) ,@body))

(defun x-mode-to-hook (mode)
  "Get the hook corresponding to MODE."
  (declare (pure t) (side-effect-free t))
  (intern (concat (symbol-name mode) "-hook")))

(defun x-mode-to-ts-mode (mode)
  "Get the tree-sitter mode corresponding to MODE."
  (declare (pure t) (side-effect-free t))
  (intern (concat
           (string-remove-suffix "-mode" (symbol-name mode))
           "-ts-mode")))

(defun x-string-split (separators string)
  "Wrapper around ‘string-split' that puts SEPARATORS first.  This makes
it convenient to use in ‘thread-last’."
  (declare (pure t) (side-effect-free t))
  (string-split string separators))

(defun x-do-and-center (function &rest arguments)
  "Call FUNCTION with ARGUMENTS and then center the screen."
  (apply function arguments)
  (recenter))

(defmacro x-comment (&rest _body)
  "Comment out BODY.  A cleaner alternative to line-commenting a region."
  (declare (indent 0))
  nil)

;;; Rational Defaults
(prefer-coding-system 'utf-8)
(savehist-mode)
(global-hl-line-mode)
(setq disabled-command-function nil)
(x-set custom-safe-themes t)

(dolist (mode #'(blink-cursor-mode show-paren-mode tooltip-mode))
  (apply mode '(-1)))

(x-set
  large-file-warning-threshold nil
  vc-follow-symlinks t
  ad-redefinition-action 'accept)

(x-set
  mouse-wheel-scroll-amount '(1 ((shift) . 1))
  mouse-wheel-progressive-speed nil
  mouse-wheel-follow-mouse t
  scroll-step 1)
(pixel-scroll-precision-mode)

(x-set show-paren-delay 0)
(dolist (hook '(conf-mode-hook prog-mode-hook))
  (add-hook hook #'show-paren-local-mode))

(x-set read-extended-command-predicate
       #'command-completion-default-include-p)

(setq-default display-line-numbers 'relative)
(line-number-mode)
(column-number-mode)

;; Backup settings
(x-set
  delete-old-versions t
  version-control t
  kept-new-versions 2
  kept-old-versions 2)

(setq-default fill-column 73)
(add-hook 'text-mode-hook #'auto-fill-mode)

(require 'autorevert)
(x-set global-auto-revert-non-file-buffers t)
(global-auto-revert-mode)

(x-set custom-file (expand-file-name
                    (format "emacs-custom-%s.el" (user-uid))
                    temporary-file-directory))
(load custom-file :noerror)

(define-key global-map [remap backward-delete-char-untabify]
            #'backward-delete-char)

;;; Documentation Improvements
(use-package helpful)

;;; Vim Emulation
(use-package evil
  :init
  (setq ;; All of the following must be set before loading ‘evil-mode’
   evil-want-Y-yank-to-eol t
   evil-v$-excludes-newline t
   evil-respect-visual-line-mode t
   evil-split-window-below t
   evil-vsplit-window-right t
   evil-want-fine-undo t
   evil-undo-system #'undo-redo
   evil-flash-delay 1
   evil-want-keybinding nil)
  (evil-mode)
  (global-visual-line-mode)
  :config
  (evil-define-operator x-evil-align-regexp (start end regexp repeat)
    "Evil operator for ‘align-regexp’."
    :move-point nil
    :restore-point t
    (interactive (let ((range (evil-operator-range)))
                   (list (car range)
                         (cadr range)
                         (concat "\\(\\s-*\\)"
                                 (read-string
                                  (format-prompt "Align regexp" nil)))
                         (y-or-n-p "Repeat? "))))
    (align-regexp start end regexp 1 1 repeat))

  (evil-define-operator x-evil-comment-or-uncomment-region (start end)
    "Evil operator for ‘comment-or-uncomment-region’."
    :move-point nil
    :restore-point t
    (comment-or-uncomment-region start end))

  (evil-define-operator x-evil-sort-lines (start end)
    "Evil operator for ‘sort-lines’."
    :move-point nil
    :restore-point t
    (sort-lines nil start end)))

(defun x-evil-surround-mode-if-evil-mode ()
  (global-evil-surround-mode (unless (evil-mode) -1)))

(use-package evil-surround
  :after evil
  :hook (evil-mode . x-evil-surround-mode-if-evil-mode)
  :init
  (x-evil-surround-mode-if-evil-mode)
  :config
  (defmacro x-evil-define-and-bind-quoted-text-object (name key start-regexp end-regexp)
    (let ((inner-name (make-symbol (concat "evil-inner-" name)))
          (outer-name (make-symbol (concat "evil-a-"     name))))
      `(progn
         (evil-define-text-object ,inner-name (count &optional beg end type)
           (evil-select-paren ,start-regexp ,end-regexp beg end type count nil))
         (evil-define-text-object ,outer-name (count &optional beg end type)
           (evil-select-paren ,start-regexp ,end-regexp beg end type count :inclusive))
         (define-key evil-inner-text-objects-map ,key #',inner-name)
         (define-key evil-outer-text-objects-map ,key #',outer-name))))

  (x-evil-define-and-bind-quoted-text-object "single-quote-open"  "‘" "‘" "’")
  (x-evil-define-and-bind-quoted-text-object "single-quote-close" "’" "‘" "’")
  (x-evil-define-and-bind-quoted-text-object "double-quote-open"  "“" "“" "”")
  (x-evil-define-and-bind-quoted-text-object "double-quote-open"  "“" "“" "”")

  (defun x-evil-surround-function ()
    "Read a function name from the minibuffer and index the list with
the selection.  This is nearly identical to ‘evil-surround-function’
except it provides a useful prompt, and is language-aware."
    (let ((list-name (or (evil-surround-read-from-minibuffer "Function name: ")
                         "")))
      (if (derived-mode-p #'lisp-mode #'lisp-data-mode #'emacs-lisp-mode)
          (cons (format "(%s " list-name) ")")
        (cons (format "%s(" (or list-name "")) ")"))))

  (defun x-evil-surround-list ()
    "Read a list name from the minibuffer and index the list with the
selection."
    (let ((list-name (evil-surround-read-from-minibuffer "List name: ")))
      (cons (format "%s[" (or list-name "")) "]")))

  (setq-default
   evil-surround-pairs-alist
   (append
    '((?‘ . ("‘ " . " ’"))
      (?’ . ("‘"  .  "’"))
      (?“ . ("“ " . " ”"))
      (?“ . ("“"  .  "”"))
      (?f . x-evil-surround-function)
      (?l . x-evil-surround-list))
    evil-surround-pairs-alist)))

(use-package evil-collection
  :after evil
  :init
  (evil-collection-init))

;;; Force Spaces For Alighment
(defun x-align-with-spaces (function &rest arguments)
  "Advice to force a given function to align using spaces instead of
tabs, regardless of the value of ‘indent-tabs-mode’."
  (let (indent-tabs-mode)
    (apply function arguments)))

(dolist (f #'(align-regexp c-backslash-region))
  (advice-add f :around #'x-align-with-spaces))

;;; Minibuffer Improvements
(use-package vertico
  :custom (vertico-cycle t)
  :init (vertico-mode))

(use-package marginalia
  :after vertico
  :init (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (orderless-matching-styles '(orderless-prefixes))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;;; Completions
;; Disable corfu for now (it’s causing Emacs to crash)
(x-comment
  (use-package corfu
    :hook ((prog-mode . corfu-mode))
    :custom
    (corfu-auto t)
    (corfu-cycle t)
    (corfu-auto-prefix 1)
    (corfu-auto-delay 0)))

(use-package company
  :init
  (with-eval-after-load 'eglot
    (add-hook 'eglot-managed-mode-hook #'company-mode))

  (defun x-company-require-prefix (candidates)
    "Transformer for ‘company-mode’ that requires that all candidates
begin with ‘company-prefix’."
    (seq-filter (lambda (s) (string-prefix-p company-prefix s)) candidates))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay (lambda () (unless (company-in-string-or-comment) 0)))
  (company-selection-wrap-around t)
  (company-tooltip-align-annotations t)
  (company-tooltip-flip-when-above t)
  (company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                       company-preview-frontend
                       company-echo-metadata-frontend))
  (company-transformers '(x-company-require-prefix
                          company-sort-by-backend-importance)))

;;; Increment- and Decrement Numbers
(defun x-increment-number-at-point (&optional arg)
  "Increment the number at point by ARG or 1 if ARG is nil.  If called
interactively, the universal argument can be used to specify ARG.  If
the number at point has leading zeros then the width of the number is
preserved."
  (interactive "*p")
  (save-excursion
    (skip-chars-backward "0123456789")
    (when (eq (char-before (point)) ?-)
      (goto-char (1- (point))))
    (save-match-data
      (when (re-search-forward "-?\\([0-9]+\\)" nil :noerror)
        (let ((answer (+ (string-to-number (match-string 0) 10)
                         (or arg 1)))
              (width (length (match-string 1))))
          (replace-match
           (format
            (concat "%0" (int-to-string (if (< answer 0) (1+ width) width)) "d")
            answer)))))))

(defun x-decrement-number-at-point (&optional arg)
  "The same as ‘x-increment-number-at-point’, but ARG is negated."
  (interactive "*p")
  (x-increment-number-at-point (- (or arg 1))))

;;; Indentation Settings
(setq-default
 tab-width 4
 indent-tabs-mode t)
(x-set evil-shift-width (default-value 'tab-width))

(defvar x-indentation-settings
  '((c-mode            :extra-vars (c-basic-offset))
    (css-mode          :extra-vars (css-indent-offset))
    (emacs-lisp-mode   :spaces t)
    (go-ts-mode        :extra-vars (go-ts-mode-indent-offset))
    (graphviz-dot-mode :extra-vars (graphviz-dot-indent-width))
    (lisp-mode         :spaces t)
    (lisp-data-mode    :spaces t)
    (org-mode          :spaces t)
    (python-mode       :extra-vars (python-indent-offset))
    (sgml-mode         :width 2 :extra-vars (sgml-basic-offset))
    (sh-mode           :extra-vars (sh-basic-offset))
    (vimscript-ts-mode :extra-vars (vimscript-ts-mode-indent-level)))
  "A list of per-mode indentation settings.  Each list contains a
major-mode and the 3 optional keyword arguments of :spaces, :width, and
:extra-vars.  When setting the settings for a given major-mode, the
settings will also be applied for that mode’s tree-sitter variant.

If :spaces is non-nil, then indentation will be performed with spaces
instead of tabs characters.

If :width is non-nil, then it will override the modes given tab-width.

If :extra-vars is non-nill, then it shall be a list of additional
mode-specific variables that need to be assigned the desired
indentation-width.")

(defun x-set-indentation-settings ()
  "Apply the indentation settings specified by ‘x-indentation-settings’."
  (interactive)
  (dolist (plist x-indentation-settings)
    (let* ((mode (car plist))
           (args (cdr plist))
           (width (or (plist-get args :width)
                      (default-value 'tab-width)))
           (spaces (or (plist-get args :spaces)
                       (not (default-value 'indent-tabs-mode))))
           (extra (plist-get args :extra-vars))
           (callback
            (λ (indent-tabs-mode (when spaces -1))
               (setq-local tab-width width
                           evil-shift-width width)
               (dolist (var extra)
                 (set var width)))))
      (dolist (hook (list (x-mode-to-hook mode)
                          (x-mode-to-hook (x-mode-to-ts-mode mode))))
		(add-hook hook callback 95)))))

(x-set-indentation-settings)

;;; Git Integration
(use-package magit
  :custom
  (magit-display-buffer-function
   #'magit-display-buffer-same-window-except-diff-v1))

(use-package magit-todos
  :after magit
  :init (magit-todos-mode))

(defun x-magit-status ()
  (interactive)
  (thread-last
    (project-current t)
    (project-root)
    (magit-status)))

(require 'project)
(add-to-list 'project-switch-commands '(x-magit-status "Git status" ?g))

;;; Tree-Sitter
(when (treesit-available-p)
  (setq treesit-language-source-alist
        '((cpp        "https://github.com/tree-sitter/tree-sitter-cpp")
          (elisp      "https://github.com/Wilfred/tree-sitter-elisp")
          (go         "https://github.com/tree-sitter/tree-sitter-go")
          (gomod      "https://github.com/camdencheek/tree-sitter-go-mod")
          (html       "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")))

  (defun x-treesit-source-sync ()
    "Install all the tree-sitter grammars in
‘treesit-language-source-alist’.  This function does not assert whether
or not the grammar is already installed, making it useful for updating
existing grammars."
    (interactive)
    (async-start
     `(lambda ()
        ,(async-inject-variables "\\`treesit-language-source-alist\\'")
        (dolist (spec treesit-language-source-alist)
          (treesit-install-language-grammar (car spec))))
     (lambda (message)
       (message "Done syncing Tree-Sitter grammars"))))

  (thread-last
    (mapcar #'car treesit-language-source-alist)
    (seq-remove #'treesit-language-available-p)
    (mapc #'treesit-install-language-grammar)))

;;; Language Server Protocol
(use-package eglot
  :hook ((c-mode      . eglot-ensure)
         (c++-mode    . eglot-ensure)
         (c-ts-mode   . eglot-ensure)
         (c++-ts-mode . eglot-ensure)
         (go-ts-mode  . eglot-ensure))
  :init
  (fset #'jsonrpc--log-event #'ignore)
  :custom
  (eglot-events-buffer 0)
  (eglot-extend-to-xref t)
  :config
  (add-hook 'eglot-managed-mode-hook (λ (eglot-inlay-hints-mode -1)))
  (dolist (feature '(eldoc flymake))
    (add-to-list 'eglot-stay-out-of feature))
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((c-mode c-ts-mode c++-mode c++-ts-mode)
                   . ("clangd" "--header-insertion=never")))))

(use-package eglot-booster
  :after eglot
  :config (eglot-booster-mode))

;;; Snippet Support
(use-package tempel
  :pin gnu
  :custom
  (tempel-trigger-prefix ",")
  (tempel-path (expand-file-name "templates" x-config-directory))
  :init
  (dolist (mode '(conf-mode prog-mode text-mode))
    (add-hook
     (x-mode-to-hook mode)
     (λ (add-hook 'completion-at-point-functions
                  #'tempel-complete -10 :local)))))

;; NOTE: This will be removed eventually, it’s only here until
;; eglot-tempel is fixed.
(use-package yasnippet
  :after eglot)

(x-comment
  (use-package eglot-tempel
    :init
    (with-eval-after-load 'eglot
      (add-hook 'eglot-managed-mode-hook
                (λ (unless (default-value eglot-tempel-mode)
                     (eglot-tempel-mode)))))))

;;; Automatically Create Directories
(defun x-auto-create-directories (original-function filename &rest arguments)
  "Automatically create and delete parent directories of files.  This
is an ‘:override’ advice for ‘find-file’ and friends.  It
automatically creates the parent directory (or directories) of the
file being visited, if necessary.  It also sets a buffer-local
variable so that the user will be prompted to delete the newly created
directories if they kill the buffer without saving it."
  (let (dirs-to-delete)
    (let* ((dir-to-create (file-name-directory filename))
           (current-dir dir-to-create))
      ;; We want to go up each directory component and add them to
      ;; ‘dirs-to-delete’ individually.
      (while (not (file-exists-p current-dir))
        (push current-dir dirs-to-delete)
        (setq current-dir (file-name-directory
                           (directory-file-name current-dir))))

      (unless (file-exists-p dir-to-create)
        (make-directory dir-to-create :parents)))

    ;; Use ‘prog1’ so that we maintain the original return value
    (prog1 (apply original-function filename arguments)
      (when dirs-to-delete
        (setq-local x-dirs-to-delete (reverse dirs-to-delete))

        ;; When we kill the buffer we want to ask if we should delete parent
        ;; directories *unless* the buffer was saved, in which case we don’t
        ;; want to do anything.
        (add-hook 'kill-buffer-hook #'x-delete-directories-if-appropriate
                  :depth :local)
        (add-hook 'after-save-hook #'x-remove-auto-directory-hooks
                  :depth :local)))))

(dolist (command #'(find-file find-alternate-file write-file))
  (advice-add command :around #'x-auto-create-directories))

(defun x-delete-directories-if-appropriate ()
  "Delete parent directories if appropriate.  This is a function for
‘kill-buffer-hook’.  If ‘x-auto-create-directories’ created the
directory containing the file for the current buffer automatically,
then offer to delete it.  Otherwise, do nothing.  Also clean up
related hooks."
  (unless (file-exists-p buffer-file-name)
    (dolist (dir-to-delete x-dirs-to-delete)
      (when (and (stringp dir-to-delete)
                 (file-exists-p dir-to-delete)
                 (thread-last
                   (directory-file-name dir-to-delete)
                   (format-prompt "Also delete directory `%s'?" nil)
                   (string-remove-suffix ": ")
                   (y-or-n-p)))
        (delete-directory dir-to-delete)))))

(defun x-remove-auto-directory-hooks ()
  "Clean up directory-deletion hooks, if necessary."
  (remove-hook 'kill-buffer-hook #'x-delete-directories-if-appropriate :local)
  (remove-hook 'after-save-hook #'x-remove-auto-directory-hooks :local))

;;; Colorize Compilation Buffer
(require 'ansi-color)
(defun x-colorize-buffer ()
  "Parse ANSI escape sequences in the current buffer."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook #'x-colorize-buffer)

;;; Autokill Complation Buffer On Success
(defun x--compilation-count-number-of-windows (&rest _arguments)
  (setq x--compilation-number-of-windows (length (window-list))))
(advice-add #'compile :before #'x--compilation-count-number-of-windows)

(defun x--kill-compilation-buffer-on-success (buffer string)
  (when (string= string "finished\n")
    (with-current-buffer buffer
      (when (> (length (window-list))
               x--compilation-number-of-windows)
        (delete-window (get-buffer-window)))
      (kill-buffer))))
(add-hook 'compilation-finish-functions #'x--kill-compilation-buffer-on-success)

;;; User Interface Themeing
(load-theme 'mango :no-confirm)
(set-fringe-style (cons 32 32))

(defvar x-alpha-background 90
  "The opacity of a graphical Emacs frame, ranging from 0–100.  A value
of 0 is fully transparent while a value of 100 is fully opaque.")

(defun x-set-alpha-background (value)
  "Set the current frames’ background opacity to VALUE."
  (interactive "NOpacity [0–100]: ")
  (set-frame-parameter nil 'alpha-background value))
(add-to-list
 'default-frame-alist (cons 'alpha-background x-alpha-background))

(defvar x-monospace-font '("Iosevka Smooth" :weight regular :height 162)
  "The default monospace font to use.  This is a list containing a
font name, font weight, and font height in that order.")

(defvar x-proportional-font '("Ysabeau" :weight light :height 180)
  "The default proportional font to use.  This is a list containing a
font name, font weight, and font height in that order.")

(defun x-set-fonts ()
  "Set the fonts specified by ‘x-monospace-font’ and
‘x-proportional-font’."
  (interactive)
  (let* ((mono-family (car x-monospace-font))
         (mono-props  (cdr x-monospace-font))
         (prop-family (car x-proportional-font))
         (prop-props  (cdr x-proportional-font))
         (mono-weight (plist-get mono-props :weight))
         (mono-height (plist-get mono-props :height))
         (prop-weight (plist-get prop-props :weight))
         (prop-height (plist-get prop-props :height)))
    (set-face-attribute 'default nil
                        :font mono-family
                        :weight mono-weight
                        :height mono-height)
    (set-face-attribute 'fixed-pitch nil
                        :font mono-family
                        :weight mono-weight
                        :height mono-height)
    (set-face-attribute 'variable-pitch nil
                        :font prop-family
                        :weight prop-weight
                        :height prop-height)))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (_frame)
                (x-set-fonts)))
  (x-set-fonts))

;; Setup ligatures
(use-package ligature
  :if
  (and (seq-contains-p (split-string system-configuration-features) "HARFBUZZ")
       (display-graphic-p))
  :init
  (defvar x-ligatures-alist
    '(((c-mode go-ts-mode)      . ("<=" ">=" "==" "!=" "*=" "__"))
      (c-mode                   . ("->")
      (go-ts-mode               . (":=" "<-"))
      ((html-mode html-ts-mode) . ("<!--" "-->" "/>")))
    "Ligatures to enable in specific modes.
Elements of this alist are of the form:

  (SPEC . LIGATURES)

where LIGATURES is a list of ligatures to enable for the set of modes
described by SPEC.

SPEC can be either a symbol, or a list of symbols.  These symbols should
correspond to modes for which the associated ligatures should be enabled.

A mode may also be specified in multiple entries.  To configure
`go-ts-mode' to have it’s set of ligatures be a super-set of the
ligatures for `c-ts-mode', the following two entries could be added:

  '((c-ts-mode go-ts-mode) . (\">=\" \"<=\" \"!=\" \"==\"))
  '(go-ts-mode             . (\":=\"))")

  (defun x-set-ligatures ()
    (interactive)
    (setq ligature-composition-table nil)
    (dolist (pair x-ligatures-alist)
      (ligature-set-ligatures (car pair) (cdr pair))))
  :config
  (x-set-ligatures)
  (global-ligature-mode))

;;; Set Project List
(defun x-set-project-list ()
  (interactive)
  (when-let ((no-dotfiles "\\`[^.]")
             (repo-directory (getenv "REPODIR"))
             (level-1 (directory-files repo-directory :full no-dotfiles)))
    (setq
     project--list
     (cl-loop for directory in level-1
              append (mapcar #'list (directory-files
                                     directory :full no-dotfiles))))
    (project--write-project-list)))

(with-eval-after-load 'project
  (x-set-project-list))

;;; C-Style
(setq-default
 c-auto-newline t
 c-hungry-delete-key t)

(defun x-c-defun-open-safe (_syntax _position)
  (if (c-cpp-define-name)
      '(after)
    '(before after)))

(defun x-c-semi&comma-after-return ()
  "‘c-mode’ criteria to avoid automatic newline insertion after entering
a semicolon following a return statement."
  (catch 'return
    (let ((end-position (point)))
      (save-excursion
        (goto-char (line-beginning-position))
        (save-match-data
          (while (re-search-forward "\\<return\\>" end-position :noerror)
            (when (eq (get-text-property (1- (point)) 'face)
                      'font-lock-keyword-face)
              (throw 'return 'stop))))))))

(c-add-style
 "mango"
 '((indent-tabs-mode            . t)
   (c-backslash-column          . 80)
   (c-backslash-max-column      . 80)
   (c-basic-offset              . 4)
   (c-block-comment-prefix      . "")
   (c-comment-only-line-offset  . 0)
   (c-label-minimum-indentation . 0)
   (c-hanging-semi&comma-criteria . (x-c-semi&comma-after-return
                                     c-semi&comma-inside-parenlist
                                     c-semi&comma-no-newlines-before-nonblanks
                                     c-semi&comma-no-newlines-for-oneline-inliners))
   (c-cleanup-list . (brace-else-brace
                      brace-elseif-brace
                      brace-catch-brace
                      comment-close-slash
                      scope-operator))
   (c-offsets-alist . ((label                 . [0])
                       (arglist-intro         . +)
                       (arglist-cont          . 0)
                       (arglist-cont-nonempty . +)
                       (arglist-close         . 0)))
   (c-hanging-braces-alist . ((defun-open . x-c-defun-open-safe)
                              (defun-close before)
                              (class-open after)
                              (class-close before)
                              (block-open after)
                              (block-close . c-snug-do-while)
                              (brace-list-open after)
                              (brace-list-close before)
                              (extern-lang-open after)
                              (extern-lang-close before)
                              (substatement-open after)
                              (statement-case-open after)))
   (c-hanging-colons-alist . ((case-label after)
                              (label after)))))
(customize-set-variable 'c-default-style "mango")

;;; Additional Mode Support
(use-package vimscript-ts-mode)
(push '("\\.go\\'" . go-ts-mode) auto-mode-alist)
(with-eval-after-load 'tempel
  (push (cons tempel-path #'lisp-data-mode) auto-mode-alist))

;;; Keybindings
(defmacro x-define-bindings (&rest body)
  (declare (indent 0))
  (let (head result keymap)
    (while body
      (setq
       head (car body)
       body (if (eq head :map)
                (progn (setq keymap (cadr body)) (cddr body))
              (push (list #'keymap-set
                          keymap
                          (car head)
                          (let ((function (cadr head)))
                            (if (symbolp function)
                                (macroexp-quote function)
                              function)))
                    result)
              (cdr body))))
    (macroexp-progn result)))

(defmacro x-define-evil-bindings (&rest body)
  (declare (indent 0))
  (let (mode-spec head result keymap prefix)
    (while body
      (setq
       head (car body)
       body (cond ((eq head :prefix)
                   (setq prefix (cadr body))
                   (cddr body))
                  ((eq head :map)
                   (setq keymap (cadr body))
                   (cddr body))
                  ((and (symbolp head) (string-prefix-p ":" (symbol-name head)))
                   (setq mode-spec
                         (thread-last
                           (symbol-name head)
                           (string-remove-prefix ":")
                           (x-string-split "&")
                           (mapcar #'intern)))
                   (cdr body))
                  (t
                   (push (list #'evil-define-key
                               (macroexp-quote mode-spec)
                               (or keymap (macroexp-quote 'global))
                               (kbd (concat "<leader>" prefix (car head)))
                               (macroexp-quote (cadr head)))
                         result)
                   (cdr body)))))
    (macroexp-progn result)))

;; <Leader> and <LocalLeader>
(evil-set-leader nil (kbd "SPC"))
(evil-set-leader nil (kbd ",") :localleader)

;; Evil bindings that aren’t namespaced under ‘<leader>’
(evil-define-key
  '(normal visual) 'global "gc" #'x-evil-comment-or-uncomment-region)
(let ((modes '(normal insert visual operator motion)))
  (evil-define-key modes 'global (kbd "C-u") (λi (x-do-and-center #'evil-scroll-up 0)))
  (evil-define-key modes 'global (kbd "C-d") (λi (x-do-and-center #'evil-scroll-down 0)))
  (evil-define-key modes 'global (kbd "C-v") #'evil-visual-line)
  (evil-define-key modes 'global (kbd "M-v") #'evil-visual-block))

(defun x-minibuffer-backward-kill (arg)
  "When minibuffer completing a filename, delete up to the parent folder,
otherwise delete a word."
  (interactive "p")
  (if minibuffer-completing-file-name
	  (if (string-match-p "/." (minibuffer-contents))
		  (zap-up-to-char (- arg) ?/)
		(delete-minibuffer-contents))
	(backward-kill-word arg)))
(keymap-set minibuffer-local-map "C-h" #'x-minibuffer-backward-kill)

(with-eval-after-load 'vertico
  (x-define-bindings
    :map vertico-map
    ("C-j" vertico-next)
    ("C-k" vertico-previous)
    ("C-l" vertico-insert)))

(defun x-company-select-candidate (pred)
  "Select either the next or previous candidate in the candidate list based on
the comparison of the ‘company-pseudo-tooltip-overlay’ height and 0 using PRED."
  (let ((ov company-pseudo-tooltip-overlay))
    (if (and ov (apply pred (list (overlay-get ov 'company-height) 0)))
        (company-select-previous)
      (company-select-next))))

(with-eval-after-load 'company
  (x-define-bindings
    :map company-active-map
    ("C-j" (λi (x-company-select-candidate #'<)))
    ("C-k" (λi (x-company-select-candidate #'>)))))

(with-eval-after-load 'tempel
  (x-define-bindings
    :map tempel-map
    ("C-l" tempel-next)
    ("C-h" tempel-previous)))

(defun x-tetris-rotate-mirror ()
  (interactive)
  (tetris-rotate-next)
  (tetris-rotate-next))

(with-eval-after-load 'tetris
  (x-define-bindings
    :map tetris-mode-map
    ("a"   tetris-move-left)
    ("d"   tetris-move-right)
    ("k"   tetris-rotate-next)
    (";"   tetris-rotate-prev)
    ("l"   tetris-move-down)
    ("o"   x-tetris-rotate-mirror)
    ("SPC" tetris-move-bottom)))

(x-define-evil-bindings
  :normal&visual
  ("a" x-evil-align-regexp)
  ("s" x-evil-sort-lines)

  :normal
  :prefix "g"
  ("g" magit-status)

  :normal
  :prefix "h"
  ("f" helpful-callable)
  ("s" helpful-symbol)
  ("k" helpful-key)
  ("p" helpful-at-point)

  :normal&visual
  :prefix "n"
  ("d" x-decrement-number-at-point)
  ("i" x-increment-number-at-point))

(with-eval-after-load 'eglot
  (x-define-evil-bindings
    :map eglot-mode-map
    :prefix "l"
    :normal
    ("a" eglot-code-actions)
    ("r" eglot-rename)))

;;; Emacs Calculator
(setq calc-display-trail nil)

;;; Emacs Tetris
(use-package tetris
  :hook (tetris-mode . (lambda () (evil-local-mode -1))))
