;;; grab.el --- Emacs integration for grab  -*- lexical-binding: t; -*-

;; Author: Thomas Voss <mail@thomasvoss.com>
;; Description: TODO
;; Keywords: matching, tools

;;; Commentary:

;; TODO

;;; Code:

(require 'ansi-color)
(require 'dired)
(require 'project)
(require 'rx)
(require 'xref)

(defgroup grab nil
  "Settings for `grab'."
  :group 'tools)

(defcustom grab-command "grab"
  "The base executable for the Grab tool."
  :type 'string)

(defcustom git-grab-command "git-grab"
  "The base executable for the Git Grab tool."
  :type 'string)

(defcustom grab-command-arguments '("-c" "-Hmulti")
  "Arguments to pass to `grab-command'."
  :type '(repeat string))

(defcustom git-grab-command-arguments grab-command-arguments
  "Arguments to pass to `git-grab-command'."
  :type '(repeat string))

(defcustom grab-default-pattern '("x// h//" . 3)
  "The default pattern in Grab prompts"
  :type '(choice (cons string natnum)
                 (string)))

(defvar grab-history nil
  "Minibuffer history for Grab search patterns.")


;;; Xref Location Class

(cl-defstruct grab-location
  "A location in a file specified by a byte offset."
  file offset)

(cl-defmethod xref-location-marker ((loc grab-location))
  "Return a marker for the grab location LOC."
  (let* ((file (grab-location-file loc))
         (offset (grab-location-offset loc))
         (buf (find-file-noselect file)))
    (with-current-buffer buf
      (save-restriction
        (widen)
        (goto-char (byte-to-position (1+ offset)))
        (point-marker)))))

(cl-defmethod xref-location-group ((loc grab-location))
  "Group matches by their file name in the xref buffer."
  (grab-location-file loc))

(cl-defmethod xref-location-line ((loc grab-location))
  "Return the position of the match.

`xref' internally performs a log on this value, so we need to handle the
0 case."
  (max 1 (grab-location-offset loc)))


;;; Process Management & Parsing

(defvar grab--header-regexp
  (rx-let ((ansi-escape (seq "\e[" (* (any "0-9;")) "m"))
           (highlighted (thing)
                        (seq (* ansi-escape)
                             thing
                             (* ansi-escape))))
    (rx line-start
        (highlighted (group (+ (not (any ?: ?\e ?\n)))))
        (highlighted ?:)
        (highlighted (group (+ digit)))
        (highlighted ?:)))
  "Regular expression matching the grab output header.")

(defun grab--format-summary (summary)
  (let* ((summary (ansi-color-apply (string-trim-right summary)))
         (pos 0)
         (len (length summary)))
    (while (< pos len)
      (let ((next (next-property-change pos summary len)))
        (when (or (get-text-property pos 'font-lock-face summary)
                  (get-text-property pos 'face summary))
          (put-text-property pos next 'font-lock-face 'xref-match summary)
          (remove-list-of-text-properties pos next '(face) summary))
        (setq pos next)))
    summary))

(defun grab--parse-output (dir)
  (let (xrefs file offset match-start)
    (goto-char (point-min))
    (while (re-search-forward grab--header-regexp nil :noerror)
      (let ((next-file (match-string-no-properties 1))
            (next-offset (string-to-number (match-string-no-properties 2)))
            (next-start (point)))
        (when file
          (let* ((summary (buffer-substring-no-properties
                           match-start (match-beginning 0)))
                 (summary (grab--format-summary summary))
                 (full-path (expand-file-name file dir))
                 (loc (make-grab-location :file full-path :offset offset)))
            (push (xref-make summary loc) xrefs)))
        (setq file next-file
              offset next-offset
              match-start next-start)))
    (when file
      (let* ((summary (buffer-substring-no-properties
                       match-start (point-max)))
             (summary (grab--format-summary summary))
             (full-path (expand-file-name file dir))
             (loc (make-grab-location :file full-path :offset offset)))
        (push (xref-make summary loc) xrefs)))
    (unless xrefs
      (user-error "No matches found for grab pattern"))
    (nreverse xrefs)))

(defun grab--directory (cmd args pattern dir)
  (grab--files cmd args pattern dir
               (directory-files-recursively dir "." nil t)))

(defun grab--files (cmd args pattern dir files)
  (lambda ()
    (let ((default-directory dir))
      (with-temp-buffer
        (apply #'call-process cmd nil t nil
               (flatten-tree (list args "--" pattern files)))
        (grab--parse-output dir)))))

(defun grab--read-pattern ()
  (read-string (format-prompt "Grab Pattern" nil)
               grab-default-pattern
               'grab-history))


;;; Interactive Commands

;;;###autoload
(defun grab (pattern)
  "Run grab with PATTERN in the current directory."
  (interactive (list (grab--read-pattern)))
  (xref-show-xrefs
   (grab--directory grab-command
                    grab-command-arguments
                    pattern
                    default-directory)
   nil))

;;;###autoload
(defun git-grab (pattern)
  "Run git grab with PATTERN in the current directory."
  (interactive (list (grab--read-pattern)))
  (xref-show-xrefs
   (grab--files git-grab-command
                git-grab-command-arguments
                pattern
                default-directory
                nil)
   nil))

;;;###autoload
(defun project-grab (pattern)
  "Run grab with PATTERN at the project root."
  (interactive (list (grab--read-pattern)))
  (let* ((project (project-current t))
         (default-directory (project-root project)))
    (xref-show-xrefs
     (grab--directory grab-command
                      grab-command-arguments
                      pattern
                      default-directory)
     nil)))

;;;###autoload
(defun project-git-grab (pattern)
  "Run git grab with PATTERN at the project root."
  (interactive (list (grab--read-pattern)))
  (let* ((project (project-current t))
         (default-directory (project-root project)))
    (xref-show-xrefs
     (grab--files git-grab-command
                  git-grab-command-arguments
                  pattern
                  default-directory
                  nil)
     nil)))

;;;###autoload
(defun dired-grab-marked-files (pattern)
  "Run grab with PATTERN on all marked files in dired."
  (interactive (list (grab--read-pattern)))
  (let* ((project (project-current t))
         (default-directory (project-root project)))
    (xref-show-xrefs
     (grab--files grab-command grab-command-arguments pattern
                  default-directory (dired-get-marked-files))
     nil)))

(provide 'grab)
;;; grab.el ends here
