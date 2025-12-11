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
  (project-switch-commands
   '((project-dired                    "Dired"      ?d)
     (project-find-file                "Find File"  ?f)
     (mm-projects-project-magit-status "Git Status" ?s)))
  :config
  (unless mm-humanwave-p
    (if-let ((repo-directory (getenv "REPODIR")))
        (let* ((list-dir
                (lambda (path)
                  (directory-files path :full "\\`[^.]")))
               (directories
                (cl-loop for author in (funcall list-dir (getenv "REPODIR"))
                         append (cl-loop for path in (funcall list-dir author)
                                         collect (list (concat path "/"))))))
          (with-temp-buffer
            (prin1 directories (current-buffer))
            (write-file project-list-file))
          (project--read-project-list))
      (warn "The REPODIR environment variable is not set."))))


;;; Emacs VC

(use-package vc-hooks
  :custom
  (vc-follow-symlinks t)
  (vc-handled-backends '(Git)))


;;; Git Client

(use-package magit
  :ensure t
  :bind (("C-c b" . magit-blame-addition)
         :map magit-status-mode-map
         ("[" . magit-section-backward-sibling)
         ("]" . magit-section-forward-sibling))
  :custom
  (git-commit-style-convention-checks
   '(non-empty-second-line overlong-summary-line))
  (git-commit-summary-max-length 50)
  (magit-diff-refine-hunk t)
  (magit-display-buffer-function
   #'magit-display-buffer-same-window-except-diff-v1)
  (transient-default-level 7)
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
    '("a" "all remotes" mm-projects-magit-push-current-to-all-remotes))
  (add-to-list 'magit-blame-styles
               '(margin
                 (show-lines       . t)
                 (margin-format    . (" %C %a" " %s%f" " "))
                 (margin-width     . 73)
                 (margin-face      . magit-blame-margin)
                 (margin-body-face . (magit-blame-dimmed)))))

(use-package magit-repos
  :ensure nil                           ; Part of ‘magit’
  :if (not mm-humanwave-p)
  :commands (magit-list-repositories)
  :init
  (if-let ((directory (getenv "REPODIR")))
      (setopt magit-repository-directories `((,directory . 2)))
    (warn "The REPODIR environment variable is not set.")))

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


;;; GitHub Pull Requests

(require 'gh)
(keymap-global-set "C-c p c" #'gh-create-pr)
(keymap-global-set "C-c p o" #'gh-open-previous-pr)

(provide 'mm-projects)
