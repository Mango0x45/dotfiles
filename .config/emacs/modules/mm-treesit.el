;;; mm-treesit.el --- Tree-Sitter configuration  -*- lexical-binding: t; -*-

(unless (treesit-available-p)
  (error "Tree-Sitter is not available."))


;;; Tree-Sitter Variables

(defvar mm-treesit-language-remap-alist
  '((cpp        . c++)
    (gomod      . go-mod)
    (javascript . js)
    (vim        . vimscript))
  "TODO")

(setopt treesit-font-lock-level 4)
(setopt treesit-language-source-alist
        '((awk        "https://github.com/Beaglefoot/tree-sitter-awk")
          (c          "https://github.com/tree-sitter/tree-sitter-c")
          (cpp        "https://github.com/tree-sitter/tree-sitter-cpp")
          (css        "https://github.com/tree-sitter/tree-sitter-css")
          (go         "https://github.com/tree-sitter/tree-sitter-go")
          (gomod      "https://github.com/camdencheek/tree-sitter-go-mod")
          (gsp        "git://git.thomasvoss.com/tree-sitter-gsp.git")
          (html       "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (python     "https://github.com/tree-sitter/tree-sitter-python")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript"
                      "master" "typescript/src")
          (vim        "https://github.com/tree-sitter-grammars/tree-sitter-vim")
          (vue        "https://github.com/ikatyang/tree-sitter-vue")))


;;; Install Missing Parsers

(defun mm-treesit-sync-sources ()
  "Sync Tree-Sitter parsers.
Reinstall the Tree-Sitter parsers specified by
 `treesit-language-source-alist'."
  (interactive)
  (let ((total (length treesit-language-source-alist))
        (count 0)
        (work treesit-language-source-alist)
        (processors-to-use (max 1 (1- (num-processors)))))
    (while work
      (let ((specs (seq-take work processors-to-use)))
        (dolist (spec specs)
          (async-start
           `(lambda ()
              ,(async-inject-variables "\\`treesit-language-source-alist\\'")
              (treesit-install-language-grammar ',(car spec)))
           (lambda (_)
             (setq count (1+ count))
             (message "Done syncing Tree-Sitter grammar for `%s' [%d/%d]"
                      (car spec) count total))))
        (setq work (seq-drop work processors-to-use))))))

(thread-last
  (mapcar #'car treesit-language-source-alist)
  (seq-remove #'treesit-language-available-p)
  (mapc #'treesit-install-language-grammar))


;;; Install Additional TS Modes

(use-package gsp-ts-mode
  :vc (:url "https://git.thomasvoss.com/gsp-ts-mode"
       :branch "master"
       :rev :newest
       :vc-backend Git)
  :ensure t)

;; NOTE: This package doesn’t autoload its ‘auto-mode-alist’ entries
(use-package vimscript-ts-mode
  :ensure t
  :mode (rx (or (seq (? (or ?. ?_)) (? ?g) "vimrc")
                ".vim"
                ".exrc")
            eos))

;; NOTE: This package doesn’t autoload its ‘auto-mode-alist’ entries
(use-package vue-ts-mode
  :vc (:url "https://github.com/8uff3r/vue-ts-mode.git"
       :branch "main"
       :rev :newest
       :vc-backend Git)
  :ensure t
  :mode "\\.vue\\'")


;;; Prefer Tree-Sitter Modes

;; NOTE: ‘go-ts-mode’ already adds itself to ‘auto-mode-alist’ but it
;; isn’t autoloaded as of 2024-09-29 so we need to do it ourselves
;; anyway.  Same goes for ‘typescript-ts-mode’.
(defvar mm-treesit-language-file-name-alist
  '((go         . "\\.go\\'")
    (go-mod     . "/go\\.mod\\'")
    (typescript . "\\.ts\\'"))
  "Alist mapping languages to their associated file-names.
This alist is a set of pairs of the form (LANG . REGEXP) where LANG is
the symbol corresponding to a major mode with the ‘-ts-mode’ suffix
removed.  REGEXP is a regular expression matching filenames for which
the associated language’s major-mode should be enabled.

This alist is used to configure `auto-mode-alist'.")

(dolist (spec treesit-language-source-alist)
  (let* ((lang (car spec))
         (lang (alist-get lang mm-treesit-language-remap-alist lang))
         (symbol-name (symbol-name lang))
         (name-mode    (intern (concat symbol-name    "-mode")))
         (name-ts-mode (intern (concat symbol-name "-ts-mode"))))
    ;; If ‘name-ts-mode’ is already in ‘auto-mode-alist’ then we don’t
    ;; need to do anything, however if that’s not the case then if
    ;; ‘name-ts-mode’ and ‘name-mode’ are both bound we do a simple
    ;; remap.  If the above is not true then we lookup the extensions in
    ;; ‘mm-treesit-language-file-name-alist’.
    (cond
     ((not (fboundp name-ts-mode))
      (warn "`%s' is missing." name-ts-mode))
     ((rassq name-ts-mode auto-mode-alist)
      nil)
     ((fboundp name-mode)
      (add-to-list 'major-mode-remap-alist (cons name-mode name-ts-mode)))
     (t          ; (and (fboundp name-ts-mode) (not (fboundp name-mode)))
      (if-let ((file-regexp
                (alist-get lang mm-treesit-language-file-name-alist)))
          (add-to-list 'auto-mode-alist (cons file-regexp name-ts-mode))
        (warn "Unable to determine the extension for `%s'." name-ts-mode))))))


;;; Hack For C23

(advice-add #'c-ts-mode--keywords :filter-return
            (defun mm-c-ts-mode-add-constexpr (keywords)
              ;; NOTE: We can’t add ‘typeof’ until it’s added to the TS grammar
              ;; https://github.com/tree-sitter/tree-sitter-c/issues/236
              (append keywords '("constexpr"))))


;;; Highlight Predefined Variables

(with-eval-after-load 'treesit
  (defvar mm-c-font-lock-rules
    (treesit-font-lock-rules
     :language 'c
     :feature 'constant
     :override t
     `(((identifier) @font-lock-constant-face
        (:match ,(rx bos (or "__func__" "__FUNCTION__") eos)
                @font-lock-constant-face))))))

(add-hook 'c-ts-mode-hook
          (defun mm-c-apply-font-lock-extras ()
            (setq treesit-font-lock-settings
                  (append treesit-font-lock-settings mm-c-font-lock-rules))))


;;; Region Expansion

(defun mm-expreg-expand (n)
  "Expand to N syntactic units."
  (interactive "p")
  (dotimes (_ n)
    (expreg-expand)))

(defun mm-expreg-expand-dwim ()
  "Do-What-I-Mean `expreg-expand' to start with symbol or word.
If over a real symbol, mark that directly, else start with a word.  Fall
back to regular `expreg-expand'."
  (interactive)
  (if (region-active-p)
      (expreg-expand)
    (let ((symbol (bounds-of-thing-at-point 'symbol)))
      (cond
       ((equal (bounds-of-thing-at-point 'word) symbol)
        (mm-expreg-expand 1))
       (symbol
        (mm-expreg-expand 2))
       (:else
        (expreg-expand))))))

(use-package expreg
  :ensure t
  :commands (mm-expreg-expand mm-expreg-expand-dwim)
  :bind ("M-SPC" . mm-expreg-expand-dwim))

(provide 'mm-treesit)