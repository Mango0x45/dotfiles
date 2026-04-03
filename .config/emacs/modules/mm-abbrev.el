;;; mm-abbrev.el --- Emacs abbreviations and templates  -*- lexical-binding: t; -*-

;;; Helpers

(defmacro mm-abbrev-define-abbreviations (table &rest definitions)
  "Define abbrevations for an abbreviation TABLE.
Expand abbrev DEFINITIONS for the given TABLE.  DEFINITIONS are a
sequence of either string pairs mapping an abbreviation to its
expansion, or a string and symbol pair mapping an abbreviation to a
function.

After adding all abbreviations to TABLE, this macro marks TABLE as
case-sensitive to avoid unexpected abbreviation expansions."
  (declare (indent 1))
  (unless (cl-evenp (length definitions))
    (user-error "expected an even number of elements in DEFINITIONS"))
  `(progn
     ,@(cl-loop for (abbrev expansion) in (seq-partition definitions 2)
                if (stringp expansion)
                  collect (list #'define-abbrev table abbrev expansion)
                else
                  collect (list #'define-abbrev table abbrev "" expansion))
     (abbrev-table-put ,table :case-fixed t)))


;;; Abbreviation Configuration

(use-package abbrev
  :hook prog-mode
  :custom
  (abbrev-file-name (expand-file-name "abbev-defs" mm-data-directory))
  (save-abbrevs 'silently))


;;; Abbreviation Definitions

(use-package python
  :if mm-humanwave-p
  :config
  (mm-abbrev-define-abbreviations python-ts-mode-abbrev-table
    "empb" "with emphasize.Block():"
    "empf" "@emphasize.func"
    "empi" "from shared.system import emphasize"
    "empt" "emphasize.this"))

(provide 'mm-abbrev)
