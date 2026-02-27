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
        '((awk
           "https://github.com/Beaglefoot/tree-sitter-awk")
          (c
           "https://github.com/tree-sitter/tree-sitter-c")
          (cpp
           "https://github.com/tree-sitter/tree-sitter-cpp")
          (css
           "https://github.com/tree-sitter/tree-sitter-css")
          (dockerfile
           "https://github.com/camdencheek/tree-sitter-dockerfile")
          (elixir
           "https://github.com/elixir-lang/tree-sitter-elixir")
          (go
           "https://github.com/tree-sitter/tree-sitter-go")
          (gomod
           "https://github.com/camdencheek/tree-sitter-go-mod")
          (gsp
           "git://git.thomasvoss.com/tree-sitter-gsp.git")
          (heex
           "https://github.com/phoenixframework/tree-sitter-heex")
          (html
           "https://github.com/tree-sitter/tree-sitter-html")
          (java
           "https://github.com/tree-sitter/tree-sitter-java")
          (javascript
           "https://github.com/tree-sitter/tree-sitter-javascript")
          (json
           "https://github.com/tree-sitter/tree-sitter-json")
          (markdown
           "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
           "split_parser" "tree-sitter-markdown/src")
          (markdown-inline
           "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
           "split_parser" "tree-sitter-markdown-inline/src")
          (python
           "https://github.com/tree-sitter/tree-sitter-python")
          (rust
           "https://github.com/tree-sitter/tree-sitter-rust")
          (tsx
           "https://github.com/tree-sitter/tree-sitter-typescript"
           "master" "tsx/src")
          (typescript
           "https://github.com/tree-sitter/tree-sitter-typescript"
           "master" "typescript/src")
          (vim
           "https://github.com/tree-sitter-grammars/tree-sitter-vim")
          (vue
           "https://github.com/ikatyang/tree-sitter-vue")
          (yaml
           "https://github.com/tree-sitter-grammars/tree-sitter-yaml")))


;;; Install Missing Parsers

(defun mm-treesit-install-all ()
  "Install all Tree-Sitter parsers.
This is like `mm-treesit-install-missing' but also reinstalls parsers
that are already installed."
  (interactive)
  (cl-loop for (lang) in treesit-language-source-alist
           do (treesit-install-language-grammar lang)))

(defun mm-treesit-install-missing ()
  "Install missing Tree-Sitter parsers.
The parsers are taken from `treesit-language-source-alist'."
  (interactive)
  (cl-loop for (lang) in treesit-language-source-alist
           unless (treesit-language-available-p lang)
           do (treesit-install-language-grammar lang)))

(mm-treesit-install-missing)


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
  :vc ( :url "https://github.com/8uff3r/vue-ts-mode.git"
        :branch "main"
        :rev :newest
        :vc-backend Git)
  :ensure t
  :mode "\\.vue\\'")

;; NOTE: This package doesn’t autoload its ‘auto-mode-alist’ entries
(use-package markdown-ts-mode
  :ensure t
  :mode "\\.md\\'")


;;; Prefer Tree-Sitter Modes

;; NOTE: ‘go-ts-mode’ already adds itself to ‘auto-mode-alist’ but it
;; isn’t autoloaded as of 2024-09-29 so we need to do it ourselves
;; anyway.  Same goes for ‘typescript-ts-mode’.
(defvar mm-treesit-language-file-name-alist
  '((elixir     . "\\.exs?\\'")
    (go         . "\\.go\\'")
    (gomod      . "/go\\.mod\\'")
    (heex       . "\\.heex\\'")
    (json       . "\\.json\\'")
    (rust       . "\\.rs\\'")
    (tsx        . "\\.tsx\\'")
    (typescript . "\\.ts\\'")
    (yaml       . "\\.ya?ml\\'"))
  "Alist mapping languages to their associated file-names.
This alist is a set of pairs of the form (LANG . REGEXP) where LANG is
the symbol corresponding to a major mode with the `-ts-mode' suffix
removed.  REGEXP is a regular expression matching filenames for which
the associated language’s major-mode should be enabled.

This alist is used to configure `auto-mode-alist'.")

(defvar mm-treesit-dont-have-modes
  '(markdown-inline)
  "List of languages that don't have modes.
Some languages may come with multiple parsers, (e.g. `markdown' and
`markdown-inline') and as a result one-or-more of the parsers won't be
associated with a mode.  To avoid breaking the configuration, these
languages should be listed here.")

(dolist (spec treesit-language-source-alist)
  (let* ((lang (car spec))
         (lang-remap (alist-get lang mm-treesit-language-remap-alist lang))
         (name-mode    (intern (format    "%s-mode" lang-remap)))
         (name-ts-mode (intern (format "%s-ts-mode" lang-remap))))
    ;; If ‘name-ts-mode’ is already in ‘auto-mode-alist’ then we don’t
    ;; need to do anything, however if that’s not the case then if
    ;; ‘name-ts-mode’ and ‘name-mode’ are both bound we do a simple
    ;; remap.  If the above is not true then we lookup the extensions in
    ;; ‘mm-treesit-language-file-name-alist’.
    (cond
     ((memq lang mm-treesit-dont-have-modes)
      nil)
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

;; JavaScript being difficult as usual
(add-to-list 'major-mode-remap-alist '(javascript-mode . js-ts-mode))


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
