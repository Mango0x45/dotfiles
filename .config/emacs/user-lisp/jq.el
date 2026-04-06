;;; jq.el --- Interact with JQ in Emacs  -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib))

(defgroup jq nil
  "Interact with JQ within Emacs."
  :group 'tools
  :prefix "jq-")

(defcustom jq-live-major-mode
  (cond ((fboundp #'json-ts-mode) #'json-ts-mode)
        ((fboundp #'js-json-mode) #'js-json-mode))
  "The major mode to use for display JSON with `jq-live'."
  :type 'function
  :group 'jq)

(defun jq--run (query json-input tabsp)
  "Run jq QUERY on JSON-INPUT string."
  (with-temp-buffer
    (insert json-input)
    (let ((args (list query)))
      (when tabsp
        (push "--tab" args))
      (let ((exit-code (apply #'call-process-region (point-min) (point-max)
                              "jq" :delete t nil args)))
        (cons exit-code (buffer-string))))))

(defun jq-live--render-preview (query json-input preview-buffer tabsp)
  "Render the live JQ preview into PREVIEW-BUFFER."
  (let ((inhibit-read-only t))
    (with-current-buffer preview-buffer
      (erase-buffer)
      (condition-case err
          (cl-destructuring-bind (exit-code . string)
              (jq--run query json-input tabsp)
            (if (zerop exit-code)
                (insert string)
              (insert (propertize string 'face 'error))
              (insert json-input)))
        (error
         (insert (format "%s\n%s"
                         (propertize (format "Error: %s" err) 'face 'error)
                         json-input))))
      (goto-char (point-min))
      (when (fboundp jq-live-major-mode)
        (funcall jq-live-major-mode))))
  (display-buffer preview-buffer))

;;;###autoload
(defun jq-filter-region (query &optional beg end)
  "Filter the region between BEG and END with a jq QUERY.
When called interactively, QUERY is read from the minibuffer, and the
active region is filtered.  If there is no active region, the whole
buffer is filtered.

For interactive filtering, see `jq-live'."
  (interactive
   (list
    (read-string (format-prompt "Query" nil))
    (when (use-region-p) (region-beginning))
    (when (use-region-p) (region-end))))
  (let* ((beg (or beg (point-min)))
         (end (or end (point-max)))
         (json-input (buffer-substring-no-properties beg end))
         (tabsp indent-tabs-mode))
    (cl-destructuring-bind (exit-code . output)
        (jq--run query json-input tabsp)
      (if (zerop exit-code)
          (atomic-change-group
            (delete-region beg end)
            (insert output)
            (indent-region beg (point))
            (message "jq applied."))
        (message "jq error: %s" output)))))

;;;###autoload
(defun jq-live (&optional beg end)
  "Filter the region between BEG and END with a live preview.
For non-interactive filtering, see `jq-filter-region'."
  (declare (interactive-only t))
  (interactive
   (list (when (use-region-p) (region-beginning))
         (when (use-region-p) (region-end))))
  (unless (executable-find "jq")
    (user-error "`jq' not found in PATH."))

  (let* ((input-buffer (current-buffer))
         (beg (or beg (point-min)))
         (end (or end (point-max)))
         (json-input (buffer-substring-no-properties beg end))
         (tabsp indent-tabs-mode)
         (preview-buffer-name (format "*JQ Preview: %s*" (buffer-name)))
         (preview-buffer (get-buffer-create preview-buffer-name))
         (last-query "")
         (update-fn
          (lambda ()
            (let ((query (minibuffer-contents)))
              (unless (equal query last-query)
                (setq last-query query)
                (jq-live--render-preview query json-input preview-buffer
                                         tabsp))))))

    (unwind-protect
        (let ((query
               (minibuffer-with-setup-hook
                   (lambda ()
                     (add-hook 'post-command-hook update-fn nil :local))
                 (read-from-minibuffer (format-prompt "Query" nil)))))
          (with-current-buffer input-buffer
            (jq-filter-region query beg end)))
      (when (buffer-live-p preview-buffer)
        (kill-buffer preview-buffer)))))

(provide 'jq)
