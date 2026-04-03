;; TODO: ‘defcustom’ this
(defvar live-jq-major-mode
  (cond ((fboundp #'json-ts-mode) #'json-ts-mode)
        ((fboundp #'json-mode) #'json-mode))
  "TODO")

(defvar live-jq--input-buffer nil
  "The buffer containing the original JSON data.")

(defvar live-jq--preview-buffer "*JQ Preview*"
  "The buffer showing the live jq results.")

(defvar live-jq--last-query "")

(defun live-jq--get-json-input ()
  "Return the contents of the input buffer as a string."
  (with-current-buffer live-jq--input-buffer
    (buffer-substring-no-properties (point-min) (point-max))))

(defun live-jq--run-jq (query)
  "Run jq QUERY on the input buffer's content and return result string or nil on error."
  (let ((json-input (live-jq--get-json-input)))
    (with-temp-buffer
      (insert json-input)
      (let ((exit-code (call-process-region
                        (point-min) (point-max)
                        "jq" :delete t nil "--tab" query)))
        (when (zerop exit-code)
          (buffer-string))))))

(defun live-jq--render-jq-preview (query)
  "Update preview buffer with the result or error of jq QUERY."
  (let* ((preview-buffer (get-buffer-create live-jq--preview-buffer))
         (json-input (live-jq--get-json-input))
         (inhibit-read-only t))
    (with-current-buffer preview-buffer
      (erase-buffer)
      (condition-case err
          (with-temp-buffer
            (insert json-input)
            (let ((exit-code (call-process-region
                              (point-min) (point-max)
                              "jq" nil preview-buffer nil "--tab" query)))
              (when (not (zerop exit-code))
                (erase-buffer)
                (insert "%s\n%s"
                        (propertize (format "jq error (exit %d): %s" exit-code query)
                                    'face 'error)
                        json-input))))
        (error
         (insert "%s\n%s"
                 (propertize (format "Error: %s" err) 'face 'error)
                 input-json)))
      (goto-char (point-min))
      (when live-jq-major-mode
        (funcall live-jq-major-mode))))
  (display-buffer live-jq--preview-buffer))

(defun live-jq--minibuffer-update ()
  "Update preview as user types."
  (let ((query (minibuffer-contents)))
    (unless (equal query live-jq--last-query)
      (setq live-jq--last-query query)
      (live-jq--render-jq-preview query))))

;;;###autoload
(defun live-jq ()
  "Prompt for a jq query, show live results, and replace buffer on confirmation."
  (interactive)
  (unless (executable-find "jq")
    (user-error "`jq' not found in PATH."))

  (setq live-jq--input-buffer (current-buffer))
  (setq live-jq--last-query "")

  ;; Clean up preview buffer if user cancels with C-g
  (let ((minibuffer-setup-hook
         (list (lambda ()
                 ;; Add post-command-hook for live preview
                 (add-hook 'post-command-hook #'live-jq--minibuffer-update nil t)
                 ;; Add abort cleanup
                 (add-hook 'minibuffer-exit-hook
                           (lambda ()
                             (when (get-buffer live-jq--preview-buffer)
                               (kill-buffer live-jq--preview-buffer)))
                           nil t)))))
    (let ((query (read-from-minibuffer (format-prompt "Query" nil))))
      (unwind-protect
          (let ((result (live-jq--run-jq query)))
            (if result
                (with-current-buffer live-jq--input-buffer
                  (let ((inhibit-read-only t))
                    (erase-buffer)
                    (insert result))
                  (message "jq applied."))
              (user-error "Invalid jq query: see *jq-preview* for details")))
        ;; Cleanup preview buffer after any outcome
        (when (get-buffer live-jq--preview-buffer)
          (kill-buffer live-jq--preview-buffer))))))

(provide 'live-jq)
