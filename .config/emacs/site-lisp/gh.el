;;; gh.el --- GitHub integration for Emacs  -*- lexical-binding: t; -*-

(defun gh-get-labels ()
  "Return a list of labels in the current GitHub repository."
  (with-temp-buffer
    (call-process "gh" nil t nil "label" "list" "--sort" "name" "--json" "name")
    (goto-char (point-min))
    (seq-map (lambda (x) (gethash "name" x))
             (json-parse-buffer))))

;; TODO: Set title and body in a buffer like Magit
(defun gh-create-pr (title &optional labels draftp)
  "Create a GitHub pull request.
If DRAFTP is non-nil, the PR will be created as a draft.

LABELS is a list of labels.  A list of available labels can be fetched
via `gh-get-labels'."
  (interactive
   (list
    (read-string (format-prompt "PR Title" nil))
    (completing-read-multiple (format-prompt "PR Labels" nil)
                              (gh-get-labels))
    (y-or-n-p "Create PR as a draft? ")))
  (let* ((project (project-name (project-current)))
         (flags `("--fill-verbose" "--assignee" "@me"))
         (label-string (mapconcat #'identity labels ",")))
    ;; TODO: Remove this
    (when (string= project "blixem")
      (setq title (format "%s %s" (car (vc-git-branches)) title))
      (when (member "Patch" labels)
        (setq flags (append flags '("--base" "release")))))
    (setq flags (append flags `("--title" ,title)))
    (when draftp
      (setq flags (append flags '("--draft"))))
    (when labels
      (setq flags (append flags `("--label" ,label-string))))
    (with-temp-buffer
      (apply #'call-process "gh" nil t nil "pr" "create" flags)
      (message (buffer-string)))))

(defvar gh-pr-regexp
  "\\`https://\\(?:www\\.\\)?github\\.com/[^/]+/[^/]+/pull/[[:digit:]]+\\'")

(defun gh--pr-link-p (s)
  (declare (pure t) (side-effect-free t))
  (string-match-p gh-pr-regexp s))

(defun gh-open-previous-pr ()
  "Open the previous GitHub pull request.
Opens the previous pull request created by `gh-create-pr' by searching
for the echoed URL in the `*Messages*' buffer."
  (interactive)
  (with-current-buffer "*Messages*"
    (goto-char (point-max))
    (while (not (gh--pr-link-p (buffer-substring-no-properties
                                (pos-bol) (pos-eol))))
      (unless (line-move -1 :noerror)
        (user-error "No previous pull request found.")))
      (browse-url-at-point)))

(provide 'gh)
