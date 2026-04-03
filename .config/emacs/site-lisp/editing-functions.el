;;; editing-functions.el --- Text editing commands  -*- lexical-binding: t; -*-

(defun ef-join-current-and-next-line (&optional arg beg end)
  "Join the current- and next lines.
This function is identical to `join-line' but it joins the current
line with the next one instead of the previous one."
  (interactive
   (progn (barf-if-buffer-read-only)
          (cons current-prefix-arg
                (and (use-region-p)
                     (list (region-beginning) (region-end))))))
  (delete-indentation
   (unless (or beg end) (not arg))
   beg end))

(defun ef-mark-entire-word (&optional arg allow-extend)
  "Mark ARG words beginning at point.
This command is a wrapper around `mark-word' that moves the point such
that the word under point is entirely marked.  ARG and ALLOW-EXTEND
are just as they are with `mark-word.'"
  (interactive "P\np")
  (if (eq last-command this-command)
      (mark-word arg allow-extend)
    (let ((bounds (bounds-of-thing-at-point 'word))
          (numeric-arg (or allow-extend 0)))
      (if bounds
          (goto-char (if (< numeric-arg 0)
                         (cdr bounds)
                       (car bounds)))
        (forward-to-word (when (< numeric-arg 0) -1))))
    (mark-word arg allow-extend)))

(defun ef-mark-entire-sexp (&optional arg allow-extend)
  "Mark ARG sexps beginning at point.
This command is a wrapper around `mark-sexp' that moves the point such
that the sexp under point is entirely marked.  ARG and ALLOW-EXTEND
are just as they are with `mark-sexp.'"
  (interactive "P\np")
  (if (eq last-command this-command)
      (mark-sexp arg allow-extend)
    (let ((bounds (bounds-of-thing-at-point 'sexp))
          (numeric-arg allow-extend))
      (if bounds
          (goto-char (if (< numeric-arg 0)
                         (cdr bounds)
                       (car bounds)))
        (if (< numeric-arg 0)
            (progn
              (backward-sexp)
              (forward-sexp))
          (forward-sexp)
          (backward-sexp))))
    (mark-sexp arg allow-extend)))

(defun ef-mark-line-dwim (&optional arg)
  "Mark ARG lines beginning at point.
If the region is active then it is extended by ARG lines.  If called
without a prefix argument this command marks one line forwards unless
point is ahead of the mark in which case this command marks one line
backwards.

If this function is called with a negative prefix argument and no
region active, the current line is marked."
  (declare (interactive-only t))
  (interactive "P")
  (let ((numeric-arg (prefix-numeric-value arg)))
    (if (region-active-p)
        (progn
          (exchange-point-and-mark)
          (goto-char (1+ (pos-eol (if arg numeric-arg
                                    (when (< (point) (mark)) -1)))))
          (exchange-point-and-mark))
      (if (< numeric-arg 0)
          (progn
            (push-mark (pos-bol (+ 2 numeric-arg)) nil :activate)
            (goto-char (1+ (pos-eol))))
        (push-mark (1+ (pos-eol numeric-arg)) nil :activate)
        (goto-char (pos-bol))))))

(defun ef-open-line (arg)
  "Insert and move to a new empty line after point.
With prefix argument ARG, inserts and moves to a new empty line before
point."
  (interactive "*P")
  (end-of-line)
  (newline-and-indent)
  (when arg
    (transpose-lines 1)
    (previous-line 2)
    (end-of-line)))

(defun ef-split-line (&optional above)
  "Split the line at point.
Place the contents after point on a new line, indenting the new line
according to the current major mode.  With prefix argument ABOVE the
contents after point are placed on a new line before point."
  (interactive "*P")
  (save-excursion
    (let* ((start (point))
           (end   (pos-eol))
           (string (buffer-substring start end)))
      (delete-region start end)
      (when above
        (goto-char (1- (pos-bol))))
      (newline)
      (insert string)
      (indent-according-to-mode))))

(defun ef-sort-dwim (&optional reversep)
  "Sort regions do-what-i-mean.
When multiple cursors are not being used this command functions just
like `sort-lines' with the start- and end bounds set to the current
region beginning and -end.

When using multiple cursors this command sorts the regions marked by
each cursor (effectively calling `emc-sort-regions'.

When called with a prefix argument REVERSEP, sorting occurs in reverse
order."
  (interactive "*P")
  (if (and (featurep 'multiple-cursors-extensions)
           (< 1 (mc/num-cursors)))
      (emc-sort-regions reversep)
    (sort-lines reversep (region-beginning) (region-end))))

(defun ef-yank (&optional arg)
  "Yank text from the kill-ring.
Yank the most recent kill from the kill ring via `yank'.  If called
with prefix argument ARG then interactively yank from the kill ring
via `yank-from-kill-ring'.

If `consult' is available than this command instead calls
`consult-yank-from-kill-ring' when called with non-nil ARG."
  (declare (interactive-only t))
  (interactive "*P")
  ;; Avoid ‘current-prefix-arg’ cascading down to ‘yank-from-kill-ring’
  (let (current-prefix-arg)
    (cond ((null arg)
           (yank))
          ((featurep 'consult)
           (call-interactively #'consult-yank-from-kill-ring))
          (:else
           (call-interactively #'yank-from-kill-ring)))))

(defun ef-search-forward-char (char &optional n)
  "Search forwards to the Nth occurance of CHAR.
If called interactively CHAR is read from the minibuffer and N is
given by the prefix argument.

If N is negative then this function searches backwards.

When searching forwards point is left before CHAR while when searching
backwards point is left after CHAR."
  (interactive
   (list (read-char)
         (prefix-numeric-value current-prefix-arg)))
  (when (and (> n 0) (= char (char-after (point))))
    (forward-char))
  (search-forward (char-to-string char) nil nil n)
  (when (> n 0)
    (backward-char)))

(provide 'editing-functions)
