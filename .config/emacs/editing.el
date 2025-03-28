;;; editing.el --- Text editing commands  -*- lexical-binding: t; -*-

(defun e/align-regexp (regexp repeat)
  "Align the marked region on REGEXP.
When called interactively REGEXP is read from the minibuffer and the
user is prompted about whether they would like to REPEAT the alignment.

This function wraps `align-regexp' and implicitly prepends REGEXP with
\"\\(\\s-*\\)\"."
  (interactive
   (progn (barf-if-buffer-read-only)
          (list (concat "\\(\\s-*\\)"
                        (read-string
                         (format-prompt "Align regexp" nil)))
                (y-or-n-p "Repeat?"))))
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

(defun e/mark-entire-word (&optional arg allow-extend)
  "Mark ARG words beginning at point.
This command is a wrapper around `mark-word' that moves the point such
that the word under point is entirely marked.  ARG and ALLOW-EXTEND are
just as they are with `mark-word.'"
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
  (interactive "*P")
  (end-of-line)
  (newline-and-indent)
  (when arg
    (transpose-lines 1)
    (previous-line 2)
    (end-of-line)))

(defun e/split-line (&optional above)
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

(defun e/mc/sort-regions (&optional reverse)
  "Sort marked regions.
This command is an exact replica of `mc/sort-regions' except that
calling this command with a prefix argument REVERSE sorts the marked
regions in reverse order."
  (interactive "*P")
  (unless (use-region-p)
    (mc/execute-command-for-all-cursors))
  (setq mc--strings-to-replace (sort (mc--ordered-region-strings)
                                     (if reverse #'string> #'string<)))
  (mc--replace-region-strings))

(defun e/sort-dwim (&optional reverse)
  "Sort regions do-what-i-mean.
When multiple cursors are not being used this command functions just
like `sort-lines' with the start- and end bounds set to the current
region beginning and -end.

When using multiple cursors this command sorts the regions marked by
each cursor (effectively calling `e/mc/sort-regions'.

When called with a prefix argument REVERSE, sorting occurs in reverse
order."
  (interactive "*P")
  (if (and (fboundp #'mc/num-cursors)
           (< 1 (mc/num-cursors)))
      (e/mc/sort-regions reverse)
    (sort-lines reverse (region-beginning) (region-end))))

(defun e/yank (&optional arg)
  "Yank text from the kill-ring.
Yank the most recent kill from the kill ring via `yank'.  If called with
prefix argument ARG then interactively yank from the kill ring via
`yank-from-kill-ring'.

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
          (t
           (call-interactively #'yank-from-kill-ring)))))

(defun mm-search-forward-char (char &optional n)
  "Search forwards to the Nth occurance of CHAR.
If called interactively CHAR is read from the minibuffer and N is given
by the prefix argument.

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

(defun mm-search-backward-char (char &optional n)
  "Search backwards to the Nth occurance of CHAR.
This function is identical to `mm-search-forward-char' with N negated."
  (declare (interactive-only t))
  (interactive
   (list (read-char)
         (prefix-numeric-value current-prefix-arg)))
  (mm-search-forward-char char (- n)))

(defun mm-add-cursor-to-next-thing (thing)
  "Add a fake cursor to the next occurance of THING.
THING is any symbol that can be given to ‘bounds-of-thing-at-point’.

If there is an active region, the next THING will be marked."
  (let ((bounds (bounds-of-thing-at-point thing)))
    (if (null bounds)
        (progn
          (forward-thing thing)
          (goto-char (car (bounds-of-thing-at-point thing))))
      (mc/save-excursion
       (when (> (mc/num-cursors) 1)
         (goto-char (overlay-end (mc/furthest-cursor-after-point))))
       (goto-char (cdr (bounds-of-thing-at-point thing)))
       (forward-thing thing)
       (let ((bounds (bounds-of-thing-at-point thing)))
         (goto-char (car bounds))
         (when (use-region-p)
           (push-mark (cdr bounds)))
         (mc/create-fake-cursor-at-point))))))

(defun mm-add-cursor-to-next-word ()
  "Add a fake cursor to the next word."
  (declare (interactive-only t))
  (interactive)
  (mm-add-cursor-to-next-thing 'word)
  (mc/maybe-multiple-cursors-mode))

(defun mm-transpose-cursor-regions (n)
  "Interchange the regions of each cursor.
With prefix arg N, the regions are rotated N places (backwards if N is
negative)."
  (interactive "p")
  (when (= (mc/num-cursors) 1)
    (user-error "Cannot transpose with only one cursor."))
  (unless (use-region-p)
    (user-error "No active region."))
  (setq mc--strings-to-replace
        (funcall (if (< n 0) #'mm-rotate-left #'mm-rotate-right)
                 (abs n) (mc--ordered-region-strings)))
  (mc--replace-region-strings))

(provide 'editing)