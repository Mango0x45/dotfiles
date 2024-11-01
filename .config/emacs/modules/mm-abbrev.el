;;; mm-abbrev.el --- Emacs abbreviations and templates  -*- lexical-binding: t; -*-

;;; Helpers

(defmacro mm-define-abbreviations (table &rest definitions)
  "Define abbrevations for an abbreviation TABLE.
Expand abbrev DEFINITIONS for the given TABLE.  DEFINITIONS are a
sequence of either string pairs mapping an abbreviation to its
expansion, or a string and symbol pair mapping an abbreviation to a
function.

After adding all abbreviations to TABLE, this macro marks TABLE as
case-sensitive to avoid unexpected abbreviation expansions."
  (declare (indent 1))
  (unless (cl-evenp (length definitions))
    (user-error "expected an even-number of elements in DEFINITIONS"))
  `(progn
     ,@(cl-loop for (abbrev expansion) in (seq-partition definitions 2)
                if (stringp expansion)
                  collect (list #'define-abbrev table abbrev expansion)
                else
                  collect (list #'define-abbrev table abbrev "" expansion))
     (abbrev-table-put ,table :case-fixed t)))


;;; Abbreviation Configuration

(use-package abbrev
  :init
  (setq-default abbrev-mode t)
  :custom
  (abbrev-file-name (expand-file-name "abbev-defs" mm-data-directory))
  (save-abbrevs 'silently))


;;; Abbreviation Definitions

(defvar mm-c-mode-abbrev-table (make-abbrev-table)
  "Abbreviations shared between `c-mode', `c++-mode', `c-ts-mode', and
`c++-ts-mode'.")

(mm-define-abbreviations mm-c-mode-abbrev-table
  "flf" "flockfile"
  "fpf" "fprintf"
  "fuf" "funlockfile"
  "pf"  "printf"
  "se"  "stderr"
  "si"  "stdin"
  "so"  "stdout")

(with-eval-after-load 'cc-mode
  (setq c-mode-abbrev-table   (copy-abbrev-table mm-c-mode-abbrev-table)
        c++-mode-abbrev-table (copy-abbrev-table mm-c-mode-abbrev-table)))
(with-eval-after-load 'c-ts-mode
  (setq c-ts-mode-abbrev-table   (copy-abbrev-table mm-c-mode-abbrev-table)
        c++-ts-mode-abbrev-table (copy-abbrev-table mm-c-mode-abbrev-table)))

(mm-define-abbreviations emacs-lisp-mode-abbrev-table
  "ald" ";;;###autoload"
  "gc"  "goto-char"
  "ins" "insert"
  "itv" "interactive"
  "mtb" "match-beginning"
  "mte" "match-end"
  "ntr" "narrow-to-region"
  "pbl" "pos-bol"
  "pel" "pos-eol"
  "pmn" "point-min"
  "pmx" "point-max"
  "pnt" "point"
  "rap" "region-active-p"
  "rb"  "region-beginning"
  "re"  "region-end"
  "rsb" "re-search-backward"
  "rsf" "re-search-forward"
  "sb"  "search-backward"
  "se"  "save-excursion"
  "sf"  "search-forward"
  "sme" "save-mark-and-excursion"
  "sr"  "save-restriction")


;;; Template Configuration

(use-package tempel
  :ensure t
  :demand t
  :pin gnu
  :bind ( :map tempel-map
          ("TAB"   . tempel-next)
          ("S-TAB" . tempel-previous))
  :custom
  (tempel-trigger-prefix "<")
  :init
  (setopt tempel-path (expand-file-name "templates" mm-config-directory))
  (dolist (mode '(conf-mode prog-mode text-mode))
    (add-hook (mm-mode-to-hook mode)
              (defun mm-setup-tempel-capf ()
                (add-hook 'completion-at-point-functions
                          #'tempel-complete -10 :local))))
  (add-to-list 'auto-mode-alist (cons tempel-path #'lisp-data-mode)))

(provide 'mm-abbrev)
