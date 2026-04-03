;;; number-format-mode.el --- Format numbers in the current buffer  -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-macs)
  (require 'seq))

(defgroup number-format nil
  "Customization group for `number-format'."
  :group 'convenience)                  ; TODO: Is this the right group?

(defcustom number-format-separator "."
  "Thousands separator to use in numeric literals."
  :type 'string
  :package-version '(number-format-mode . "1.0.0")
  :group 'number-format)

(defcustom number-format-predicate nil
  "Function determining if a number should be formatted.
When formatting a number, this function is called with the START and END
range of the number in the buffer.  If this function returns non-nil the
number is formatted.

If this function is nil then all numbers are formatted."
  :type 'function
  :package-version '(number-format-mode . "1.0.0")
  :group 'number-format)

(defvar-local number-format--overlays (make-hash-table :test 'eq))
(defconst number-format--regexp "\\b[0-9]\\{4,\\}\\b")

(defun number-format--add-separators (s)
  (while (string-match "\\(.*[0-9]\\)\\([0-9][0-9][0-9].*\\)" s)
    (setq s (concat (match-string 1 s)
                    number-format-separator
                    (match-string 2 s))))
  s)

(defun number-format--adjust-overlays (ov _1 beg end &optional _2)
  (let* ((ov-beg (overlay-start ov))
         (ov-end (overlay-end   ov))
         (overlays (overlays-in ov-beg ov-end)))
    (mapcar #'delete-overlay (gethash ov number-format--overlays))
    (save-excursion
      (goto-char ov-beg)
      (if (looking-at number-format--regexp :inhibit-modify)
          (puthash ov (number-format--at-range ov-beg ov-end)
                   number-format--overlays)
        (delete-overlay ov)
        (remhash ov number-format--overlays)))))

(defun number-format--at-range (beg end)
  (when (or (null number-format-predicate)
            (funcall number-format-predicate beg end))
    (let* ((offsets [3 1 2])
           (len (- end beg))
           (off (aref offsets (mod len 3))))
      (goto-char (+ beg off)))
    (let (overlays)
      (while (< (point) end)
        (let* ((group-end (+ (point) 3))
               (ov (make-overlay (point) group-end)))
          (overlay-put ov 'before-string ".")
          (overlay-put ov 'evaporate t)
          (push ov overlays)
          (goto-char group-end)))
      overlays)))

(defun number-format--jit-lock (beg end)
  (let ((line-beg (save-excursion (goto-char beg) (line-beginning-position)))
	(line-end (save-excursion (goto-char end) (line-end-position))))
    (number-unformat-region line-beg line-end)
    (number-format-region   line-beg line-end)))

;;;###autoload
(defun number-format-region (beg end)
  "Format numbers between BEG and END.
When called interactively, format numbers in the active region."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (save-restriction
      (narrow-to-region beg end)
      (number-unformat-region beg end)
      (while (re-search-forward number-format--regexp nil :noerror)
        (save-excursion
          (cl-destructuring-bind (beg end) (match-data)
            (let ((ov (make-overlay beg end nil nil :rear-advance)))
              (overlay-put ov 'evaporate t)
              (dolist (sym '(insert-behind-hooks
                             insert-in-front-hooks
                             modification-hooks))
                (overlay-put ov sym '(number-format--adjust-overlays)))
              (puthash ov (number-format--at-range beg end)
                       number-format--overlays))))))))

;;;###autoload
(defun number-unformat-region (beg end)
  "Unformat numbers between BEG and END.
When called interactively, unformat numbers in the active region."
  (interactive "r")
  (dolist (ov (overlays-in beg end))
    (when-let ((overlays (gethash ov number-format--overlays)))
      (mapcar #'delete-overlay overlays)
      (remhash ov number-format--overlays)
      (delete-overlay ov))))

;;;###autoload
(defun number-format-buffer ()
  "Format numbers in the current buffer."
  (interactive)
  (number-format-region (point-min) (point-max)))

;;;###autoload
(defun number-unformat-buffer ()
  "Unformat numbers in the current buffer."
  (interactive)
  (number-unformat-region (point-min) (point-max)))

;;;###autoload
(define-minor-mode number-format-mode
  "TODO"
  :lighter " Number-Format"
  :group 'number-format
  (number-unformat-buffer)
  (if number-format-mode
      (jit-lock-register #'number-format--jit-lock)
    (jit-lock-unregister #'number-format--jit-lock)))

(provide 'number-format)