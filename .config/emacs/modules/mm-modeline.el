(defface mm-modeline-modified
  '((t :inherit font-lock-warning-face :weight bold))
  "Face for unsaved changes in the modeline.")

(defface mm-modeline-read-only
  '((t :inherit font-lock-comment-face :weight bold))
  "Face for read-only status in the modeline.")

(defface mm-modeline-narrowed
  '((t :inherit font-lock-string-face :weight bold))
  "Face for narrowed buffer status.")

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

;; Emacs 30: Tell the right-align feature to align to the window edge
(setq-default mode-line-right-align-edge 'window)

(setq-default mode-line-format
              '(
                "%e" ; Prints memory/eval error messages if they occur

                ;; 3. Read-only & 7. Unsaved changes
                (:eval
                 (cond
                  (buffer-read-only
                   (propertize " RO " 'face 'mm-modeline-read-only))
                  ((buffer-modified-p)
                   (propertize " ** " 'face 'mm-modeline-modified))
                  (t
                   (propertize " -- " 'face 'shadow))))

                ;; Buffer Name (Added for basic usability)
                " "
                (:propertize "%b" face bold)

                ;; Version Control (VC) Status
                (:eval
                 (when vc-mode
                   (propertize (substring-no-properties vc-mode) 'face 'mm-modeline-vc)))
                " "

                ;; 4. Narrowing Status
                (:eval
                 (when (buffer-narrowed-p)
                   (propertize " Narrowed " 'face 'mm-modeline-narrowed)))

                ;; 5. Overwrite Mode Status
                (:eval
                 (when overwrite-mode
                   (propertize " OVR " 'face 'mm-modeline-overwrite)))

                ;; 6. Active Region Line Count
                (:eval
                 (when (use-region-p)
                   (let ((lines (count-lines (region-beginning) (region-end))))
                     (propertize (if (= lines 1)
                                     "1 line selected "
                                   (format "%d lines selected " lines))
                                 'face 'mm-modeline-region))))

                ;; --- EMACS 30 MAGIC ---
                ;; Everything after this symbol is pushed to the right margin!
                mode-line-format-right-align

                ;; 1. Major Mode
                (:propertize " %m " face mm-modeline-major-mode)

                ;; Separator
                " | "

                ;; 2. Cursor Position (Line & Column)
                (:propertize "%l:%c " face mm-modeline-position)
                ))

(provide 'mm-modeline)
