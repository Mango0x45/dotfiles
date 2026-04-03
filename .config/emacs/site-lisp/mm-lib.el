;;; mm-lib.el --- Helper functions and macros  -*- lexical-binding: t; -*-

(defun mm-mode-to-hook (mode)
  "Get the hook corresponding to MODE."
  (declare (ftype (function (symbol) symbol))
           (pure t) (side-effect-free t))
  (intern (concat (symbol-name mode) "-hook")))

(defun mm-mode-to-ts-mode (mode)
  "Get the Tree-Sitter mode corresponding to MODE."
  (declare (ftype (function (symbol) symbol))
           (pure t) (side-effect-free t))
  (intern (concat
           (string-remove-suffix "-mode" (symbol-name mode))
           "-ts-mode")))

(defun mm-ts-mode-to-mode (ts-mode)
  "Get the non-Tree-Sitter mode corresponding to TS-MODE."
  (declare (ftype (function (symbol) symbol))
           (pure t) (side-effect-free t))
  (intern (concat
           (string-remove-suffix "-ts-mode" (symbol-name ts-mode))
           "-mode")))

(defsubst mm-string-split (separators string)
  "Split STRING on SEPARATORS.
Wrapper around `string-split' that puts separators first.  This makes it
convenient to use in `thread-last'."
  (declare (ftype (function (string string) (list string)))
           (pure t) (side-effect-free t))
  (string-split string separators))

(defun mm-as-number (string-or-number)
  "Ensure STRING-OR-NUMBER is a number.
If given a number return STRING-OR-NUMBER as-is, otherwise convert it to
a number and then return it.

This function is meant to be used in conjuction with `read-string' and
`format-prompt'."
  (declare (ftype (function (or string number) number))
           (pure t) (side-effect-free t))
  (if (stringp string-or-number)
      (string-to-number string-or-number)
    string-or-number))

(defun mm-camel-to-lisp (string)
  "Convert STRING from camelCase to lisp-case."
  (declare (ftype (function (string) string))
           (pure t) (side-effect-free t))
  (let ((case-fold-search nil))
    (downcase
     (replace-regexp-in-string
      (rx (group (or lower digit)) (group upper)) "\\1-\\2" string))))

(defun mm-do-and-center (function &rest arguments)
  "Call FUNCTION with ARGUMENTS and then center the screen."
  (apply function arguments)
  (when (called-interactively-p)
    (recenter)))

(defmacro mm-comment (&rest _body)
  "Comment out BODY.
A cleaner alternative to line-commenting a region."
  (declare (indent 0))
  nil)

(defun mm-nil (&rest _)
  "Return nil."
  nil)

(defmacro mm-with-suppressed-output (&rest body)
  "Execute BODY while suppressing output.
Execute BODY as given with all output to the echo area or the *Messages*
buffer suppressed."
  (declare (indent 0))
  `(let ((inhibit-message t)
         (message-log-max nil))
     ,@body))

(provide 'mm-lib)
