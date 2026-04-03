;;; multiple-cursors-extensions.el --- Extensions to multiple-cursors.el  -*- lexical-binding: t; -*-

;;; Helper functions
(defun mce--rotate-left (n list)
  "Rotate the elements of LIST N places to the left."
  (declare (ftype (function (number (list t)) (list t)))
           (pure t) (side-effect-free t))
  (append (nthcdr n list) (butlast list (- (length list) n))))

(defun mce--rotate-right (n list)
  "Rotate the elements of LIST N places to the right."
  (declare (ftype (function (number (list t)) (list t)))
           (pure t) (side-effect-free t))
  (mce--rotate-left (- (length list) n) list))

(defmacro mce--define-marking-command (name search-function noun)
  (let ((noun-symbol (intern noun)))
    `(defun ,name (beg end ,noun-symbol)
       ,(format "Mark all occurances of %s between BEG and END.
If called interactively with an active region then all matches in the
region are marked, otherwise all matches in the buffer are marked."
                (upcase noun))
       (interactive
        (list (or (use-region-beginning) (point-min))
              (or (use-region-end) (point-max))
              (read-string
               (format-prompt ,(concat "Match " noun) nil))))
       (if (string-empty-p ,noun-symbol)
           (message "Command aborted")
         (catch 'mce--no-match
           (mc/remove-fake-cursors)
           (goto-char beg)
           (let (did-match-p)
             (while (,search-function ,noun-symbol end :noerror)
               (setq did-match-p t)
               (push-mark (match-beginning 0))
               (exchange-point-and-mark)
               (mc/create-fake-cursor-at-point)
               (goto-char (mark)))
             (unless did-match-p
               (message "No match for `%s'" ,noun-symbol)
               (throw 'mce--no-match nil)))
           (when-let ((first (mc/furthest-cursor-before-point)))
             (mc/pop-state-from-overlay first))
           (multiple-cursors-mode (if (> (mc/num-cursors) 1)
                                      1
                                    0)))))))


;;; Public API

(defun mce-transpose-cursor-regions (n)
  "Interchange the regions of each cursor.
With prefix arg N, the regions are rotated N places (backwards if N is
negative)."
  (interactive "*p")
  (when (= (mc/num-cursors) 1)
    (user-error "Cannot transpose with only one cursor."))
  (unless (use-region-p)
    (user-error "No active region."))
  (setq mc--strings-to-replace
        (funcall (if (< n 0)
                     #'mce--rotate-left
                   #'mce--rotate-right)
                 (abs n)
                 (mc--ordered-region-strings)))
  (mc--replace-region-strings))

(mce--define-marking-command mce-mark-all-in-region
                             search-forward
                             "string")
(mce--define-marking-command mce-mark-all-in-region-regexp
                             re-search-forward
                             "regexp")

(defun mce-add-cursor-to-next-thing (thing)
  "Add a fake cursor to the next occurance of THING.
THING is any symbol that can be given to `bounds-of-thing-at-point'.

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

(defun mce-add-cursor-to-next-word ()
  "Add a fake cursor to the next word."
  (declare (interactive-only t))
  (interactive)
  (mce-add-cursor-to-next-thing 'word)
  (mc/maybe-multiple-cursors-mode))

(defun mce-add-cursor-to-next-symbol ()
  "Add a fake cursor to the next symbol."
  (declare (interactive-only t))
  (interactive)
  (mce-add-cursor-to-next-thing 'symbol)
  (mc/maybe-multiple-cursors-mode))

(defun emc-sort-regions (&optional reversep)
  "Sort marked regions.
This command is an exact replica of `mc/sort-regions' except that
calling this command with a prefix argument REVERSE sorts the marked
regions in reverse order."
  (interactive "*P")
  (unless (use-region-p)
    (user-error "No active region."))
  (setq mc--strings-to-replace (sort (mc--ordered-region-strings)
                                     (if reversep #'string> #'string<)))
  (mc--replace-region-strings))

(provide 'multiple-cursors-extensions)
