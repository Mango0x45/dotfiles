;;; mm-completion.el --- Configuration for Emacs completion  -*- lexical-binding: t; -*-


;;; Vertical Completions

(use-package vertico
  :ensure t
  :hook after-init
  :custom
  (vertico-cycle t)
  :config
  (require 'hl-line)
  ;; When working with ‘file-name-shadow-mode’ enabled, if I shadow old
  ;; input (i.e.) by typing ‘~/’ after ‘foo/bar’ Vertico will clear the
  ;; old path to keep only the current input (the old path is hidden by
  ;; ‘rfn-shadow’ anyways).
  (with-eval-after-load 'rfn-shadow
    (add-hook 'rfn-shadow-update-overlay-hook #'vertico-directory-tidy)))


;;; Annotate Completions

(use-package marginalia
  :ensure t
  :hook after-init
  :config
  (with-eval-after-load 'magit
    (defvar mm-marginalia--magit-cache nil)
    (add-hook 'minibuffer-setup-hook
              (lambda () (setq mm-marginalia--magit-cache nil)))

    (defvar-local mm-marginalia-magit-base-branch "master")

    (defface mm-diffstat-counter-added
      '((t :inherit magit-diffstat-added))
      "TODO")
    (defface mm-diffstat-counter-removed
      '((t :inherit magit-diffstat-removed))
      "TODO")

    (defun mm-marginalia-populate-magit-cache ()
      "Batch-fetch all Git branch descriptions and stats into the cache."
      (setq mm-marginalia--magit-cache (make-hash-table :test #'equal))
      (when-let ((default-directory (magit-toplevel)))
        (dolist (line (magit-git-lines "config" "list"))
          (when (string-match "^branch\\.\\(.*?\\)\\.description=\\(.*\\)$" line)
            (puthash (match-string 1 line)
                     (list :desc (match-string 2 line) :stats "")
                     mm-marginalia--magit-cache)))
        (dolist (line (magit-git-lines
                       "for-each-ref"
                       (format
                        "--format=%%(refname:short)\x1F%%(ahead-behind:%s)"
                        mm-marginalia-magit-base-branch)
                       "refs/heads/"))
          (when (string-match (rx bol (group (1+ (not #x1F)))
                                  #x1F (group (1+ digit))
                                  " " (group (1+ digit)) eol)
                              line)
            (let* ((branch (match-string 1 line))
                   (ahead  (+ (string-to-number (match-string 2 line))))
                   (behind (- (string-to-number (match-string 3 line))))
                   (ahead-str (if (zerop ahead)
                                  ""
                                (propertize (format "%+d" ahead)
                                            'face 'mm-diffstat-counter-added)))
                   (behind-str (if (zerop behind)
                                   ""
                                 (propertize (format "%+d" behind)
                                             'face 'mm-diffstat-counter-removed)))
                   (stats-str (format "%5s %5s" ahead-str behind-str))
                   (existing (gethash branch mm-marginalia--magit-cache
                                      (list :desc "" :stats ""))))
              (puthash branch (plist-put existing :stats stats-str)
                       mm-marginalia--magit-cache))))))

    (defun mm-marginalia-annotate-magit-branch (cand)
      "Annotate Git branch CAND with ahead/behind stats and description."
      (unless mm-marginalia--magit-cache
        (mm-marginalia-populate-magit-cache))
      (let* ((data (gethash cand mm-marginalia--magit-cache '(:desc "" :stats "")))
             (desc  (or (plist-get data :desc)  ""))
             (stats (or (plist-get data :stats) "")))
        (marginalia--fields
         (stats :width 10)
         (desc :truncate 1.0 :face 'marginalia-documentation))))

    (add-to-list 'marginalia-annotators
                 '(magit-branch mm-marginalia-annotate-magit-branch builtin none))
    (dolist (cmd '(magit-branch-and-checkout
                   magit-branch-checkout
                   magit-branch-delete
                   magit-checkout
                   magit-merge
                   magit-rebase-branch))
      (add-to-list 'marginalia-command-categories (cons cmd 'magit-branch))))
  :custom
  (marginalia-field-width 50)
  (marginalia-max-relative-age 0))


;;; Minibuffer Completion Styles

(use-package minibuffer
  :bind ( :map minibuffer-local-completion-map
          ("SPC" . nil)
          ("?"   . nil))
  :custom
  (completion-styles '(basic substring orderless))
  (completion-category-defaults nil)    ; Avoid needing to override things
  (completion-category-overrides
   '((file             (styles . (basic partial-completion orderless)))
     (bookmark         (styles . (basic substring)))
     (library          (styles . (basic substring)))
     (imenu            (styles . (basic substring orderless)))
     (consult-location (styles . (basic substring orderless)))
     (kill-ring        (styles . (basic substring orderless)))))
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t))

(use-package orderless
  :ensure t
  :after minibuffer
  :custom
  (orderless-matching-styles '(orderless-prefixes orderless-regexp)))


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


;;; Completion Popups

(mm-comment
  (use-package corfu
    :ensure t
    :hook prog-mode
    :bind ( :map corfu-map
            ("C-<return>" . newline))
    :custom
    (corfu-auto t)
    (corfu-cycle t)
    (corfu-auto-prefix 1)
    (corfu-auto-delay .1)
    (corfu-min-width 20)
    :config
    ;; I complete with RET and this interferes with ‘tempel-next’
    (keymap-unset corfu-map "TAB" :remove)
    (with-eval-after-load 'savehist
      (corfu-history-mode)
      (add-to-list 'savehist-additional-variables 'corfu-history))
    (with-eval-after-load 'multiple-cursors
      (add-to-list 'mc/unsupported-minor-modes #'corfu-mode))))


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
  (consult-find-args
   (string-join
    '("find . -not ("
      "        -path '*/.git*'    -prune"
      "    -or -path '*/vendor/*' -prune"
      ")")
    " ")))


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

(defun mm-cape-file--not-dot-path-p (cand)
  (declare (ftype (function (string) boolean))
           (pure t) (side-effect-free t))
  (not (or (string= cand "./")
           (string= cand "../"))))

(use-package cape
  :ensure t
  :init
  (add-hook 'completion-at-point-functions
            (cape-capf-predicate #'cape-file #'mm-cape-file--not-dot-path-p))
  (add-hook 'completion-at-point-functions
            (cape-capf-prefix-length #'cape-dabbrev 3)))


;;; Completion at Point Live Completions

(use-package completion-preview
  :hook (after-init . global-completion-preview-mode)
  :custom
  (completion-preview-minimum-symbol-length 1))

(provide 'mm-completion)
