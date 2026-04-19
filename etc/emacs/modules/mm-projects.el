;;; mm-projects.el --- Configuration for project management  -*- lexical-binding: t; -*-

;;; Project Configuration

(use-package project
  :config
  (unless mm-humanwave-p
    ;; TODO: Speed this up
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
      (warn "The REPODIR environment variable is not set.")))
  :custom
  (project-list-file (expand-file-name "projects" mm-data-directory)))


;;; Version Control Support

(use-package vc-hooks
  :custom
  (vc-follow-symlinks t)
  (vc-handled-backends '(Git)))


;; Project Compilation

(use-package compile
  :config
  (require 'ansi-color)
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter))


;;; GitHub Pull Requests

;; PKG-INTERN
(use-package gh
  :bind (("C-c p c" . #'gh-create-pr)
         ("C-c p o" . #'gh-open-previous-pr))
  :commands (gh-create-pr gh-open-previous-pr))

(provide 'mm-projects)
