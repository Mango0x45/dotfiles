;;; mm-humanwave.el --- Humanwave extras  -*- lexical-binding: t; -*-


;;; Query the Backend

(defvar mm--humanwave-query-history nil
  "History for endpoints given to `mm-humanwave-query'.")

(defun mm-humanwave-query (endpoint &optional method)
  "Query and display the result of an HTTP reqiest on ENDPOINT.
If METHOD is nil, a GET request is performed."
  (interactive
   (let* ((query (read-string (format-prompt "Query" nil)
                              (car-safe mm--humanwave-query-history)
                              'mm--humanwave-query-history))
          (parts (string-split (string-trim query) " " :omit-nulls)))
     (when (length> parts 2)
       (user-error "Queries must be of the form `METHOD ENDPOINT' or `ENDPOINT'."))
     (nreverse parts)))
  (let* ((project-root (project-root (project-current :maybe-prompt)))
         (qry-path (expand-file-name "qry" project-root))
         extras)
    (unless (file-executable-p qry-path)
      (user-error "No `qry' executable found in the project root"))
    (let ((output-buffer (get-buffer-create "*Query Response*")))
      (with-current-buffer output-buffer
        (delete-region (point-min) (point-max))
        (call-process qry-path nil t nil
                      (string-trim endpoint) "-X" (or method "GET"))
        (unless (eq major-mode 'json-ts-mode)
          (json-ts-mode))
        (goto-char (point-min)))
      (display-buffer output-buffer))))

(keymap-set project-prefix-map "q" #'mm-humanwave-query)


;;; IMenu Support for Handlers

(defvar mm-humanwave-handler--regexp
  (rx bol
      (* blank)
      (or "if" "elif")
      (* blank)
      (or "topic" "schedule")
      (* blank)
      "=="
      (* blank)
      (or (seq ?\' (* (not ?\')) ?\')
          (seq ?\" (* (not ?\")) ?\"))
      (* blank)
      ?:
      (* blank)
      eol))

(defun mm-humanwave-handler-topic-imenu-index ()
  (let ((tree-index (when (fboundp 'treesit-simple-imenu--generic-function)
                      (treesit-simple-imenu--generic-function
                       treesit-simple-imenu-settings)))
        (topic-index '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward mm-humanwave-handler--regexp nil :noerror)
        (let ((label (match-string 0))
              (pos (match-beginning 0)))
          (push (cons (format "Topic: %s" (string-trim label)) pos) topic-index))))
    (append (nreverse topic-index) tree-index)))

(defun mm-humanwave-handler-topic-imenu-setup ()
  "Setup custom imenu index for `python-ts-mode'."
  (when (and (string-match-p "handlers?" (or (buffer-file-name) ""))
             (derived-mode-p #'python-ts-mode))
    (setq-local imenu-create-index-function
                #'mm-humanwave-handler-topic-imenu-index)))

(add-hook 'after-change-major-mode-hook
          #'mm-humanwave-handler-topic-imenu-setup)


;;; Insert Imports in Vue


(defun mm-humanwave-insert-vue-import-path (base-directory target-file)
  "Insert an import directive at POINT.
The import directive imports TARGET-FILE relative from BASE-DIRECTORY.
When called interactively BASE-DIRECTORY is the directory of the current
open Vue file and TARGET-FILE is a file in the current project that is
queried interactively.

When called interactively the prefix argument can be used to emulate the
behaviour of the INCLUDE-ALL-P argument to `mm-project-read-file-name'."
  (interactive
   (list
    default-directory
    (mm-humanwave-project-read-file-name current-prefix-arg)))
  (let ((path (file-name-sans-extension
               (file-relative-name target-file base-directory))))
    (insert "import ")
    (save-excursion
      (insert (format " from '%s';" path)))))

(defun mm-humanwave-project-read-file-name (&optional include-all-p)
  "Prompt for a project file.
This function is similar to `project-find-file', but it returns the path
to the selected file instead of opening it.

When called interactively the selected file is printed to the minibuffer,
otherwise it is returned.

The prefix argument INCLUDE-ALL-P is the same as the INCLUDE-ALL argument
to the `project-find-file' command."
  (interactive "P")
  (let* ((project (project-current :maybe-prompt))
         (root (project-root project))
         (files (if include-all-p
                    (let ((vc-ignores (mapcar
                                       (lambda (dir) (concat dir "/"))
                                       vc-directory-exclusion-list)))
                      (project--files-in-directory root vc-ignores))
                  (project-files project)))
         (canditates (mapcar (lambda (f) (file-relative-name f root))
                             files))
         (table (lambda (string predicate action)
                  (if (eq action 'metadata)
                      '(metadata (category . file))
                    (complete-with-action action canditates string predicate))))
         (default-directory root)
         (choice (completing-read (format-prompt "Find project file" nil)
                                  table nil :require-match)))
    (let ((path (expand-file-name choice root)))
      (if (called-interactively-p 'any)
          (message "%s" path)
        path))))


(provide 'mm-humanwave)
