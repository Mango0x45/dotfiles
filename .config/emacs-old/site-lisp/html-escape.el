;;; html-escape.el --- HTML escaping functions  -*- lexical-binding: t; -*-

(defgroup html-escape nil
  "Customization group for `html-escape'."
  :group 'convenience)

(defvar html-escape-table
  (let ((table (make-hash-table :test #'eq)))
    (puthash ?&  "&amp;"  table)
    (puthash ?<  "&lt;"   table)
    (puthash ?>  "&gt;"   table)
    (puthash ?\" "&quot;" table)
    (puthash ?'  "&#39;"  table)
    table)
  "Hash table mapping character codes to their HTML entity equivalents.")

;;;###autoload
(defun html-escape ()
  "HTML escape text in the current buffer.

Perform HTML escaping on the text in the current buffer.  If the region
is active then only escape the contents of the active region."
  (declare (interactive-only t))
  (interactive)
  (if (use-region-p)
      (html-escape-region (region-bounds))
    (html-escape-region-1 (pos-bol) (pos-eol)))
  (when (region-active-p)
    (deactivate-mark)))

(defun html-escape-region (bounds)
  "HTML escape text in the current buffer within BOUNDS.

BOUNDS takes the same form as the return value of `region-bounds'.  This
function is prefered as it supports noncontiguous regions, but there also
exists `html-escape-region-1' with a simpler bounds interface."
  (dolist (x bounds) (html-escape-region-1 (car x) (cdr x))))

(defun html-escape-region-1 (beg end)
  "HTML escape text in the current buffer within BEG and END.

This function is the same as the prefered `html-escape-region', but takes
BEG and END parameters instead of a BOUNDS parameter.  For noncontiguous
region support use `html-escape-region'."
  (save-restriction
    (narrow-to-region beg end)
    (save-excursion
      (goto-char (point-min))
      (save-match-data
        (while (re-search-forward "[&<>\"']" nil :noerror)
          (let* ((char (char-after (match-beginning 0)))
                 (replacement (gethash char html-escape-table)))
            (replace-match replacement)))))))

(provide 'html-escape)
