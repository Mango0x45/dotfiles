;;; editing.el --- Text editing commands  -*- lexical-binding: t; -*-

(defun e/align-regexp (regexp repeat)
  "Align the marked region on REGEXP.
When called interactively REGEXP is read from the minibuffer and the
user is prompted about whether they would like to REPEAT the alignment.

This function wraps `align-regexp' and implicitly prepends REGEXP with
\"\\(\\s-*\\)\"."
  (interactive
   (list (concat "\\(\\s-*\\)"
                 (read-string
                  (format-prompt "Align regexp" nil)))
         (y-or-n-p "Repeat?")))
  (let ((start (min (mark) (point)))
        (end   (max (mark) (point))))
    (align-regexp start end regexp 1 1 repeat)))

(defun e/join-current-and-next-line (&optional arg beg end)
  "Join the current- and next lines.
This function is identical to `join-line' but it joins the current line
with the next one instead of the previous one."
  (interactive
   (progn (barf-if-buffer-read-only)
          (cons current-prefix-arg
                (and (use-region-p)
                     (list (region-beginning) (region-end))))))
  (delete-indentation
   (unless (or beg end) (not arg))
   beg end))

(defun e/transpose-previous-chars ()
  "Transpose the two characters preceeding point.
This command is similar to `transpose-chars' except it transposes the
two characters that preceed the point instead of the characters that
surround it."
  (interactive)
  (save-excursion
    (backward-char)
    (transpose-chars 1)))

(defun e/transpose-current-and-next-lines ()
  "Transpose the current and next lines.
This command is similar to `transpose-lines' except it transposes the
current and next lines instead of the current and previous lines.  This
maintains symmetry with `transpose-words'."
  (interactive)
  (let ((column (current-column)))
    (forward-line)
    (transpose-lines 1)
    (forward-line -1)
    (indent-region (pos-bol) (pos-eol))
    (move-to-column column)))

(defun e/mark-entire-word (&optional arg allow-extend)
  "Mark ARG words beginning at point.
This command is a wrapper around `mark-word' that moves the point such
that the word under point is entirely marked.  ARG and ALLOW-EXTEND are
just as they are with `mark-word.'"
  (interactive "P\np")
  (if (eq last-command this-command)
      (mark-word arg allow-extend)
    (let ((bounds (bounds-of-thing-at-point 'word))
          (numeric-arg allow-extend))
      (if bounds
          (goto-char (if (< numeric-arg 0)
                         (cdr bounds)
                       (car bounds)))
        (forward-to-word (when (< numeric-arg 0) -1))))
    (mark-word arg allow-extend)))

(defun e/mark-entire-sexp (&optional arg allow-extend)
  "Mark ARG sexps beginning at point.
This command is a wrapper around `mark-sexp' that moves the point such
that the sexp under point is entirely marked.  ARG and ALLOW-EXTEND are
just as they are with `mark-sexp.'"
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

(defun e/mark-line-dwim (&optional arg)
  "Mark ARG lines beginning at point.
If the region is active then it is extended by ARG lines.  If called
without a prefix argument this command marks one line forwards unless
point is ahead of the mark in which case this command marks one line
backwards.

If this function is called with a negative prefix argument and no region
active, the current line is marked."
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

(defun e/kill-ring-save-dwim ()
  "Save the region as if killed, but don't kill it.
This function is the same as `kill-ring-save' in Transient Mark mode,
but when there is no active region it saves the line at point to the kill
ring excluding any potential trailing newline."
  (interactive)
  (if (region-active-p)
      (kill-ring-save -1 -1 :region)
    (kill-ring-save (pos-bol) (pos-eol))))

(defun e/scroll-down ()
  "Scroll down one page.
This function is identical to `cua-scroll-down' except it recenters the
screeen after scrolling.  If the user scrolls to the top of the document
then no recentering occurs."
  (interactive)
  (let ((line-number (line-number-at-pos)))
    (cua-scroll-down)
    (when (= line-number (line-number-at-pos))
      (goto-char (point-min)))
    (recenter)))

(defun e/scroll-up ()
  "Scroll up one page.
This function is identical to `cua-scroll-up' except it recenters the
screen after scrolling."
  (interactive)
  (mm-do-and-center #'cua-scroll-up))

(defun e/open-line (arg)
  "Insert and move to a new empty line after point.
With prefix argument ARG, inserts and moves to a new empty line before
point."
  (interactive "P")
  (end-of-line)
  (newline-and-indent)
  (when arg
    (transpose-lines 1)
    (previous-line 2)
    (end-of-line)))

(provide 'editing)
