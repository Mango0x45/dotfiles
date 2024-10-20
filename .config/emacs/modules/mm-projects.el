;;; mm-projects.el --- Configuration for project management  -*- lexical-binding: t; -*-

;;; Project Configuration

(defun mm-projects-project-magit-status ()
  "Open a Git status buffer for the current project.
This is intended to be called interactively via
 `project-switch-commands'."
  (interactive)
  (thread-last
    (project-current t)
    (project-root)
    (magit-status-setup-buffer)))

(use-package project
  :custom
  (project-switch-commands '((project-dired                    "Dired" ?d)
                             (project-find-file                "Find File")
                             (project-find-regexp              "Find Regexp")
                             (mm-projects-project-magit-status "Git Status" ?s)))
  :config
  (unless mm-darwin-p
    (if-let ((repo-directory (getenv "REPODIR")))
        (mm-with-suppressed-output
          (thread-last
            (directory-files repo-directory :full "\\`[^.]")
            (mapcar (lambda (path) (concat path "/"))) ; Avoid duplicate entries
            (mapc #'project-remember-projects-under)))
      (warn "The REPODIR environment variable is not set."))))


;;; Git Client

(use-package magit
  :ensure t
  :custom
  (transient-default-level 7)
  (magit-display-buffer-function
   #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (transient-define-suffix mm-projects-magit-push-current-to-all-remotes (args)
    "Push the current branch to all remotes."
    :if #'magit-get-current-branch
    (interactive (list (magit-push-arguments)))
    (run-hooks 'magit-credential-hook)
    (let ((branch (magit-get-current-branch)))
      (dolist (remote (magit-list-remotes))
        (magit-run-git-async
         "push" "-v" args remote
         (format "refs/heads/%s:refs/heads/%s" branch branch)))))
  (transient-append-suffix #'magit-push '(1 -1)
    '("a" "all remotes" mm-projects-magit-push-current-to-all-remotes)))

(use-package magit-todos
  :ensure t
  :after magit
  :hook magit-mode
  :custom
  (magit-todos-exclude-globs '("vendor/")))


;; Project Compilation

(use-package compile
  :config
  (require 'ansi-color)
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter))

(provide 'mm-projects)
