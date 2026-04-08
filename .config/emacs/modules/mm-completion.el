;;; mm-completion.el --- Configuration for Emacs completion  -*- lexical-binding: t; -*-

;;; Vertical Completions

(defun mm-completion-sw-highlight-matches (string words)
  "Apply the standard completion face to all WORDS in STRING."
  (let ((s (copy-sequence string))
        (case-fold-search completion-ignore-case))
    (dolist (word words)
      (let ((start 0)
            (regexp (regexp-quote word)))
        (while (string-match regexp s start)
          (add-face-text-property (match-beginning 0) (match-end 0)
                                  'completions-common-part nil s)
          (setq start (match-end 0)))))
    s))

(defun mm-completion-sw-all-completions (string table pred point)
  "Match candidates containing all space-separated words in STRING."
  (let* ((words (split-string string " " t))
         (completion-regexp-list (mapcar #'regexp-quote words))
         (candidates (all-completions "" table pred)))
    (if words
        (mapcar (lambda (cand) (mm-completion-sw-highlight-matches cand words))
                candidates)
      candidates)))

(defun mm-completion-sw-try-completion (string table pred point)
  "Return STRING if it matches any candidates."
  (when (mm-completion-sw-all-completions string table pred point)
    (cons string point)))

(use-package icomplete
  :hook (after-init . icomplete-vertical-mode)
  :bind ( :map icomplete-minibuffer-map
          ("TAB" . #'icomplete-force-complete)
          ("RET" . #'icomplete-force-complete-and-exit))
  :config
  (setq icomplete-scroll t)             ; Not ‘defcustom’
  :custom
  (icomplete-show-matches-on-no-input t)
  (icomplete-compute-delay 0))


;;; Annotate Completions

;; PKG-EXTERN
(use-package marginalia
  :ensure t
  :hook after-init
  :custom
  (marginalia-field-width 50)
  (marginalia-max-relative-age 0))


;;; Minibuffer Completion Styles

(use-package minibuffer
  :bind ( :map minibuffer-local-completion-map
          ("SPC" . nil)
          ("?"   . nil))
  :config
  (add-to-list 'completion-styles-alist
               '(spaced-words
                 mm-completion-sw-try-completion
                 mm-completion-sw-all-completions
                 "Orderless matching of space-separated words."))
  :custom
  (completion-styles '(basic substring spaced-words))
  (completion-category-defaults nil)   ; Avoid needing to override things
  (completion-category-overrides
   '((file             (styles . (basic partial-completion)))
     (bookmark         (styles . (basic substring)))
     (library          (styles . (basic substring)))
     (imenu            (styles . (basic substring)))
     (consult-location (styles . (basic substring)))
     (kill-ring        (styles . (basic substring)))))
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t))


;;; Disable Minibuffer Recursion Level

(use-package mb-depth
  :hook (after-init . minibuffer-depth-indicate-mode)
  :custom
  (enable-recursive-minibuffers t))


;;; Don’t Show Defaults After Typing

;; Usually if a minibuffer prompt has a default value you can access by
;; hitting RET, the prompt will remain even if you begin typing (meaning
;; the default will no longer take effect on RET).  Enabling this mode
;; disables that behaviour.

(use-package minibuf-eldef
  :hook (after-init . minibuffer-electric-default-mode)
  :custom
  (minibuffer-default-prompt-format " [%s]"))


;;; Hide Shadowed Filepaths

(use-package rfn-eshadow
  :hook (after-init . file-name-shadow-mode)
  :custom
  (file-name-shadow-properties '(invisible t intangilble t)))


;;; Save Minibuffer History

(use-package savehist-mode
  :hook (after-init . savehist-mode)
  :custom
  (history-length 200)
  (history-delete-duplicates t)
  :config
  (add-to-list 'savehist-additional-variables 'kill-ring))


;;; Enhanced Replacements for Builtins

;; TODO: Investigate other commands
;; PKG-EXTERN
(use-package consult
  :ensure t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind ( ([remap switch-to-buffer] . consult-buffer)
          ([remap imenu]            . consult-imenu)
          ([remap goto-line]        . consult-goto-line)
          ("M-F"                    . consult-focus-lines)
          :map project-prefix-map
          ("b" . consult-project-buffer)
          :map consult-narrow-map
          ("?" . consult-narrow-help))
  :custom
  (consult-async-min-input 1)
  (consult-async-split-style nil)
  (consult-async-input-debounce 0)
  (consult-async-input-throttle 0)
  (consult-find-args "find . -not ( -path */.git/* -prune -path */vendor -prune -path */node_modules -prune )"))


;;; Dynamic Abbreviations

(use-package dabbrev
  :commands (dabbrev-completion dabbrev-expand)
  :custom
  (dabbrev-upcase-means-case-search t))


;;; Finding Things

(use-package find-func
  :custom
  (find-library-include-other-files nil))


;;; Completion at Point Functions

(defun mm-completions--cape-file-not-dot-path-p (cand)
  (declare (ftype (function (string) boolean))
           (pure t) (side-effect-free t))
  (not (or (string= cand "./")
           (string= cand "../"))))

;; PKG-EXTERN
(use-package cape
  :ensure t
  :init
  (add-hook 'completion-at-point-functions
            (cape-capf-predicate
             #'cape-file
             #'mm-completions--cape-file-not-dot-path-p))
  (add-hook 'completion-at-point-functions
            (cape-capf-prefix-length #'cape-dabbrev 3)))


;;; Completion at Point Live Completions

(use-package completion-preview
  :hook (after-init . global-completion-preview-mode)
  :custom
  (completion-preview-minimum-symbol-length 1))

(use-package completion-preview
  :after multiple-cursors
  :config
  (add-hook 'multiple-cursors-mode-hook
            (defun mm-completion-set-toggle-previews-on-multiple-cursors ()
              (global-completion-preview-mode
               (when multiple-cursors-mode -1)))))

(provide 'mm-completion)
