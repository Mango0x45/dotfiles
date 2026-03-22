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


;;; IMenu Support for Handlers

(require 'imenu)
(require 'which-func)

(defvar mm-humanwave-handler--regexp
  (rx bol
      (* blank)
      (or "if" "elif")
      (* blank)
      (or "topic" "schedule")
      (* blank)
      "=="
      (* blank)
      (or ?\' ?\")
      (group (+ (not (or ?\' ?\"))))
      (or ?\' ?\")
      (* blank)
      ?:
      (* blank)
      eol))

(defun mm-humanwave-handler--insert-entry (topic-index function-parts route pos)
  (if (null function-parts)
      (cons (cons (format "%s (route)" route) pos) topic-index)
    (let* ((current-group (car function-parts))
           (rest-parts    (cdr function-parts))
           (existing-sublist (assoc current-group topic-index)))
      (if existing-sublist
          (progn
            (setcdr existing-sublist
                    (mm-humanwave-handler--insert-entry
                     (cdr existing-sublist) rest-parts route pos))
            topic-index)
        (cons (cons current-group
                    (mm-humanwave-handler--insert-entry
                     nil rest-parts route pos))
              topic-index)))))

(defun mm-humanwave-handler-topic-imenu-index ()
  (let ((case-fold-search nil)
        (tree-index (python-imenu-treesit-create-index))
        (topic-index '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward mm-humanwave-handler--regexp nil :noerror)
        (let ((route (match-string-no-properties 1))
              (pos (match-beginning 0))
              (function-parts (split-string (which-function) "\\.")))
          (setq topic-index (mm-humanwave-handler--insert-entry
                             topic-index function-parts route pos)))))
    (append (nreverse topic-index) tree-index)))

(defun mm-humanwave-handler-topic-imenu-setup ()
  "Setup custom imenu index for `python-ts-mode'."
  (when (and (string-match-p "/handlers?" (or (buffer-file-name) ""))
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
    (unless (string-match-p "/" path)
      (setq path (concat "./" path)))
    (insert "import ")
    (save-excursion
      (insert (thread-last
                (file-name-base path)
                (mm-string-split "-")
                (mapconcat #'capitalize)))
      (push-mark (point))
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

(defun mm-humanwave-insert-last-commit-message ()
  "TODO"
  (interactive)
  (insert
   (with-temp-buffer
     (call-process "git" nil t nil "log" "-1" "--pretty=%s")
     (goto-char (point-min))
     (replace-regexp "\\`HW-[0-9]+ " "")
     (string-trim (buffer-string)))))


;;; Jira Integration

(use-package jira
  :ensure t
  :custom
  (jira-api-version 3)
  (jira-base-url "https://humanwave.atlassian.net")
  (jira-detail-show-announcements nil)
  (jira-issues-max-results 100)
  (jira-issues-table-fields '(:key :status-name :assignee-name :summary))
  (jira-token-is-personal-access-token nil))


;;; Icon Autocompletion

(defvar mm-humanwave-icon-component-file "web/src/components/icon.vue"
  "Path to the <icon /> component definition.")

(defun mm-humanwave--find-icon-map ()
  (let* ((project (project-current :maybe-prompt))
         (path (expand-file-name mm-humanwave-icon-component-file
                                 (project-root project))))
    (unless (file-exists-p path)
      (user-error "File `%s' does not exist." path))
    (with-current-buffer (find-file-noselect path)
      (let* ((parser (treesit-parser-create 'typescript))
             (root-node (treesit-parser-root-node parser))
             (query `((((lexical_declaration
                         (variable_declarator
                          name: (identifier) @name)) @the_catch)
                       (:equal @name "ICON_MAP"))
                      (((variable_declaration
                         (variable_declarator
                          name: (identifier) @name)) @the_catch)
                       (:equal @name "ICON_MAP"))))
             (captures (treesit-query-capture root-node query))
             (found-node (alist-get 'the_catch captures)))
        found-node))))

(defun mm-humanwave--icon-list (found-node)
  (let ((captures (treesit-query-capture found-node '((pair) @the_pair)))
        (pairs nil))
    (when captures
      (dolist (capture captures)
        (let* ((pair-node (cdr capture))
               (key-node (treesit-node-child-by-field-name pair-node "key"))
               (val-node (treesit-node-child-by-field-name pair-node "value")))
          (when (and key-node val-node)
            (push (cons (mm-camel-to-lisp
                         (treesit-node-text key-node :no-property))
                        (treesit-node-text val-node :no-property))
                  pairs))))
      (sort pairs :key #'car :lessp #'string<))))

(defun mm-humanwave-insert-icon-component ()
  "Insert an icon at point with completion.

This command provides completion for the available props that can be
given to the <icon /> component.  The parser searches for the `ICON_MAP'
definition in the file specified by `mm-humanwave-icon-component-file'."
  (interactive "" vue-ts-mode)
  (if-let* ((node (mm-humanwave--find-icon-map))
            (alist (mm-humanwave--icon-list node)))
      (let* ((max-key-width
              (thread-last
                alist
                (mapcar (lambda (pair) (length (car pair))))
                (apply #'max)
                (+ 4)))
             (completion-extra-properties
              `(:annotation-function
                ,(lambda (key)
                   (concat
                    (propertize " "
                                'display `(space :align-to ,max-key-width))
                    (propertize (cdr (assoc key alist))
                                'face 'font-lock-string-face)))))
             (prompt (format-prompt "Icon" nil))
             (icon (completing-read prompt alist nil :require-match)))
        (insert (format "<icon %s />" icon)))
    (error "Unable to find ICON_MAP definitions")))

(provide 'mm-humanwave)
