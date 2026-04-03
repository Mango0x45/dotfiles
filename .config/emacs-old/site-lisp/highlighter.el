;;; highlighter.el --- In-buffer highlighting commands  -*- lexical-binding: t; -*-

(require 'seq)

(defgroup highlighter nil
  "Customization group for `highlighter'."
  :group 'convenience)

(defcustom highlighter-default-face 'match
  "The default face used by `highlighter-mark'."
  :type 'face
  :package-version '(highlighter . "1.0.0")
  :group 'highlighter)

(defun highlighter-mark (arg)
  "Highlight text in the buffer.
Highlight the current line or region if it is active.  Text is
highlighted using the face specified by `highlighter-default-face'.

With ARG, interactively pick a face to highlight with."
  (declare (interactive-only t))
  (interactive "P")
  (let ((bounds (if (use-region-p)
                    (region-bounds)
                  `((,(pos-bol) . ,(pos-eol)))))
        (face (when arg
                (highlighter--read-face-name "Highlight with face" #'facep))))
    (highlighter-mark-region bounds face))
  (when (region-active-p)
    (deactivate-mark)))

(defun highlighter-unmark (arg)
  "Remove highlights in the buffer.

Remove highlights from the current line or region if it is active.

With ARG, interactively pick a face.  Only highlights using the chosen
face will be removed."
  (declare (interactive-only t))
  (interactive "P")
  (let ((bounds (if (use-region-p)
                    (region-bounds)
                  `((,(pos-bol) . ,(pos-eol)))))
        (face (when arg
                (highlighter--read-face-name
                 "Clear highlights using face"
                 #'highlighter--used-face-p))))
    (highlighter-unmark-region bounds face))
  (when (region-active-p)
    (deactivate-mark)))

(defun highlighter-mark-region (bounds &optional face)
  "Highlight text in the buffer within BOUNDS.
BOUNDS uses the same format as returned by `region-bounds'.

Text is highlighted using the face specified by `highlighter-default-face'.

If FACE is nil or omitted, `highlighter-default-face' is used."
  (dolist (x bounds) (highlighter--mark-region (car x) (cdr x) face)))

(defun highlighter-unmark-region (bounds &optional face)
  "Remove highlights in the buffer within BOUNDS.
BOUNDS uses the same format as returned by `region-bounds'.

If FACE is non-nil, only remove highlights using FACE."
  (dolist (x bounds) (highlighter--unmark-region (car x) (cdr x) face)))

(defun highlighter--mark-region (beg end &optional face)
  (let ((ov (make-overlay beg end nil :front-advance))
        (face (or face highlighter-default-face 'match)))
    (overlay-put ov 'priority 1)
    (overlay-put ov 'face face)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'highlighter--mark-p t)
    (overlay-put ov 'highlighter--face face)))

(defun highlighter--unmark-region (beg end &optional face)
  (if face
      (remove-overlays beg end 'highlighter--face face)
    (remove-overlays beg end 'highlighter--mark-p t)))

(defun highlighter-unmark-buffer (arg)
  "Remove highlights in the buffer.

With ARG, interactively pick a face.  Only highlights using the chosen
face will be removed."
  (declare (interactive-only t))
  (interactive "P")
  (let ((face (when arg
                (highlighter--read-face-name
                 "Clear highlights using face"
                 #'highlighter--used-face-p))))
    (highlighter--unmark-region (point-min) (point-max) face)))

(defun highlighter--read-face-name (prompt face-predicate)
  (let (default defaults)
    (let ((prompt (format "%s: " prompt))
          (completion-extra-properties
           `(:affixation-function
             ,(lambda (faces)
                (mapcar
                 (lambda (face)
                   (list face
                         (concat (propertize read-face-name-sample-text
                                             'face face)
                                 "\t")
                         ""))
                 faces))))
          aliasfaces nonaliasfaces faces)
      ;; Build up the completion tables.
      (mapatoms (lambda (s)
                  (when (apply face-predicate s nil)
                    (if (get s 'face-alias)
                        (push (symbol-name s) aliasfaces)
                      (push (symbol-name s) nonaliasfaces)))))
      (let ((face (completing-read
                   prompt
                   (completion-table-in-turn nonaliasfaces aliasfaces)
                   nil t nil 'face-name-history defaults)))
        (when (facep face) (if (stringp face)
                               (intern face)
                             face))))))

(defun highlighter--used-face-p (face)
  (seq-filter (lambda (ov) (eq face (overlay-get ov 'highlighter--face)))
              (overlays-in (point-min) (point-max))))

(provide 'highlighter)
