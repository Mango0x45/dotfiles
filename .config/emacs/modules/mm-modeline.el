;;; mm-modeline.el --- Modeline configuration  -*- lexical-binding: t; -*-

(defface mm-modeline-modified
  '((t :inherit font-lock-warning-face :weight bold))
  "Face for unsaved changes in the modeline.")

(defface mm-modeline-read-only
  '((t :inherit font-lock-comment-face :weight bold))
  "Face for read-only status in the modeline.")

(defface mm-modeline-narrowed
  '((t :inherit font-lock-string-face :weight bold))
  "Face for narrowed buffer status.")

(defface mm-modeline-recording-macro
  '((t :inherit font-lock-warning-face :weight bold))
  "Face for recording macro status.")

(defface mm-modeline-overwrite
  '((t :inherit font-lock-builtin-face :weight bold))
  "Face for overwrite mode status.")

(defface mm-modeline-region
  '((t :inherit region :weight bold))
  "Face for the active region line counter.")

(defface mm-modeline-vc
  '((t :inherit font-lock-type-face :weight bold))
  "Face for version control status in the modeline.")

(defface mm-modeline-position
  '((t :inherit font-lock-constant-face))
  "Face for the cursor position.")

(defface mm-modeline-major-mode
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for the major mode.")

(defun mm-modeline--selection ()
  (declare (ftype (function () (cons number number)))
           (side-effect-free t))
  (let ((lines (count-lines (region-beginning) (region-end)))
        (chars (- (region-end) (region-beginning))))
    (when (bound-and-true-p multiple-cursors-mode)
      (dolist (cursor (mc/all-fake-cursors))
        (when-let* ((_ (overlay-get cursor 'mark-active))
                    (beg (overlay-get cursor 'mark))
                    (end (overlay-get cursor 'point))
                    (r-beg (min beg end))
                    (r-end (max beg end)))
          (cl-incf lines (count-lines r-beg r-end))
          (cl-incf chars (- r-end r-beg)))))
    (cons lines chars)))

(setq-default mode-line-right-align-edge 'right-margin)
(setq-default mode-line-format
              '("%e" ; Prints memory/eval error messages if they occur

                (:eval
                 (when (buffer-narrowed-p)
                   (propertize " NRW" 'face 'mm-modeline-narrowed)))

                (:eval
                 (when defining-kbd-macro
                   (propertize " REC" 'face 'mm-modeline-recording-macro)))

                (:eval
                 (when overwrite-mode
                   (propertize " OVR" 'face 'mm-modeline-overwrite)))

                (:eval
                 (cond
                  (buffer-read-only
                   (propertize " RO " 'face 'mm-modeline-read-only))
                  ((buffer-modified-p)
                   (propertize " ** " 'face 'mm-modeline-modified))
                  (t
                   (propertize " -- " 'face 'shadow))))

                (:propertize "%b" face bold)
                (:propertize " %m" face mm-modeline-major-mode)

                mode-line-format-right-align

                (:eval
                 (when (use-region-p)
                   (cl-destructuring-bind (lines . chars) (mm-modeline--selection)
                     (propertize (format "%d:%d " lines chars)
                                 'face 'mm-modeline-region))))

                (:propertize "%l:%c " face mm-modeline-position)

                (:eval
                 (when vc-mode
                   (concat
                    (propertize (string-trim (substring-no-properties vc-mode))
                                'face 'mm-modeline-vc)
                    " ")))))

(provide 'mm-modeline)
