;;; surround.el --- Surround a region with delimeters  -*- lexical-binding: t; -*-

(require 'cl-macs)

(defgroup surround nil
  "Customization group for `surround'."
  :group 'convenience)

(defcustom surround-with-paired-bracket-p t
  "Surround text with paired brackets.
If non-nil surrounding text with a character (assuming that character is
not configured in `surround-pairs-alist') will attempt to surround the
text with the supplied character and its paired bracket.

As an example, if `surround-with-paired-bracket-p' is non-nil and the
user attempts to surround the word “foo” with the character “｢” the
result would be “｢foo｣”.

Whether or not an opening- or closing bracket is provided is not
important; the opening bracket will always be placed at the front of the
region and the closing bracket at the end of the region (assuming
left-to-right writing).

In more technical terms this function surrounds text with both the
provided character and the characters corresponding Bidi_Paired_Bracket
Unicode property."
  :type 'boolean
  :package-version '(surround . "1.0.0")
  :group 'surround)

(defcustom surround-with-mirror-p t
  "Surround text with mirrored characters.
If non-nil surrounding text with a character (assuming that character is
not configured in `surround-pairs-alist') will attempt to surround the
text with the supplied character and its mirror.

As an example, if `surround-with-mirror-p' is non-nil and the user
attempts to surround the word “foo” with the character “«” the result
would be “«foo»”.

Note that unlike `surround-with-paired-bracket-p', because there is no
concept of an “opening” or “closing” bracket — because this option
doesn't work in terms of brackets — ordering matters.  This means that
surrounding “Ελλάδα” with “«” will result in “«Ελλάδα»” while
surrounding “Österreich” with “»” will result in “»Österreich«”.

In more technical terms this function surrounds text with both the
provided character and the characters corresponding Bidi_Mirroring_Glyph
Unicode property."
  :type 'boolean
  :package-version '(surround . "1.0.0")
  :group 'surround)

(defvar surround-pairs-alist '((emacs-lisp-mode
                                . ((?` ("`" . "'")))))
  "TODO")

(defun surround--get-pair-from-alist (char)
  (declare (ftype (function (char) (cons string string)))
           (side-effect-free t))
  (catch 'surround--break
    (let ((char-as-string (char-to-string char)))
      (dolist (pair surround-pairs-alist)
        (let ((mode-or-t (car pair))
              (pairs     (cdr pair)))
          (when (or (derived-mode-p mode-or-t)
                    (eq t mode-or-t))
            (dolist (pair pairs)
              (let ((open-or-trigger (car pair))
                    (closing-or-pair (cdr pair)))
                (if (numberp open-or-trigger) ; Implies trigger
                    (when (= char open-or-trigger)
                      (throw 'surround--break (car closing-or-pair)))
                  (when (string= char-as-string open-or-trigger)
                    (throw 'surround--break pair)))))))))))

(defun surround--get-pair (char)
  (declare (ftype (function (char) (cons string string)))
           (side-effect-free t))
  (or (surround--get-pair-from-alist char)
      (let ((char (char-to-string char))
            (other (char-to-string
                    (or (when surround-with-paired-bracket-p
                          (get-char-code-property char 'paired-bracket))
                        (when surround-with-mirror-p
                          (get-char-code-property char 'mirroring))
                        char)))
            (bracket-type (get-char-code-property char 'bracket-type)))
        (pcase bracket-type
          ((or 'c 'n) (cons other char))
          ('o         (cons char other))))))

(defun surround--region (pair beginning end)
  (save-excursion
    (goto-char beginning)
    (insert (car pair))
    (goto-char end)
    (insert (cdr pair))))

(defun surround-region (char)
  (interactive
   (list (read-char-from-minibuffer
          (format-prompt "Surround with" nil))))
  (when-let ((pair (surround--get-pair char)))
    (dolist (bounds (cl-loop for (beginning . end) in (region-bounds)
                             collect (cons (set-marker (make-marker) beginning)
                                           (set-marker (make-marker) end))))
      (surround--region pair (car bounds) (cdr bounds)))))

(defun surround-padded-region (char)
  (interactive
   (list (read-char-from-minibuffer
          (format-prompt "Surround with" nil))))
  (when-let ((pair (surround--get-pair char))
             (pair (cons (concat (car pair) " ")
                         (concat " " (cdr pair)))))
    (dolist (bounds (cl-loop for (beginning . end) in (region-bounds)
                             collect (cons (set-marker (make-marker) beginning)
                                           (set-marker (make-marker) end))))
      (surround--region pair (car bounds) (cdr bounds)))))

(provide 'surround)
