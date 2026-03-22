;;; grab.el --- Emacs integration for grab -*- lexical-binding: t; -*-

;; Author: Thomas Voss <mail@thomasvoss.com>
;; Description: TODO
;; Keywords: matching, tools

;;; Commentary:
;; TODO

;;; Code:

(require 'ansi-color)
(require 'compile)
(require 'project)

(defgroup grab nil
  "Settings for `grab'."
  :group 'tools)

(defcustom grab-command "grab"
  "The base executable for the grab tool."
  :type 'string)

(defcustom git-grab-command "git-grab"
  "The base executable for the git-grab tool."
  :type 'string)

(defcustom grab-command-arguments '("-c" "-Halways")
  "TODO"
  :type '(repeat string))

(defcustom git-grab-command-arguments grab-command-arguments
  "TODO"
  :type '(repeat string))

(defcustom grab-default-pattern '("x// h//" . 3)
  "TODO"
  :type '(choice (cons string natnum)
                 (string)))

(defvar grab--header-regexp
  "^\\([^:\n]+\\):\\([0-9]+\\):")

(defvar grab-results-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "RET" #'grab-goto-match)
    (keymap-set map "n"   #'grab-next-match)
    (keymap-set map "p"   #'grab-prev-match)
    map)
  "Keymap for navigating grab matches.")

(define-derived-mode grab-results-mode compilation-mode "Grab"
  "TODO"
  (setq-local compilation-error-regexp-alist nil
              compilation-error-regexp-alist-alist nil
              next-error-function #'grab-next-error)
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter nil :local)
  (font-lock-add-keywords nil
    `((,grab--header-regexp
       (1 'compilation-info)
       (2 'compilation-line-number)))))


;;; Process Management

(defun grab--run-process (command-args buffer-name)
  (let ((cmd-string (mapconcat #'shell-quote-argument command-args " ")))
    (compilation-start cmd-string
                       #'grab-results-mode
                       (lambda (_) buffer-name))))


;;; Navigation & Core Logic

(defun grab--valid-match-p ()
  "Return non-nil if the current line is a match."
  (save-excursion
    (beginning-of-line)
    (and (looking-at grab--header-regexp)
         (not (string-prefix-p "Grab started"  (match-string 1)))
         (not (string-prefix-p "Grab finished" (match-string 1))))))

(defun grab-display-match (&optional select-window-p)
  "Parse current line and display the match in the source buffer."
  (interactive "P")
  (save-excursion
    (beginning-of-line)
    (if (grab--valid-match-p)
        (let* ((file (match-string-no-properties 1))
               (offset (string-to-number (match-string-no-properties 2)))
               (full-path (expand-file-name file default-directory)))
          (if (not (file-exists-p full-path))
              (error "File `%s' does not exist" full-path)
            (let* ((buffer (find-file-noselect full-path))
                   (window (display-buffer
                            buffer '(display-buffer-reuse-window
                                     display-buffer-pop-up-window))))
              (with-selected-window window
                (let ((pos (or (byte-to-position (1+ offset)) (1+ offset))))
                  (goto-char pos)
                  (when (fboundp 'pulse-momentary-highlight-one-line)
                    (pulse-momentary-highlight-one-line pos))))
              (when select-window-p
                (select-window window)))))
      (user-error "No match found on the current line"))))

(defun grab-goto-match ()
  "Go to match on current line and select its window."
  (interactive)
  (grab-display-match :select-window-p))

(defun grab--search-forward ()
  (catch 'found
    (while (re-search-forward grab--header-regexp nil :noerror)
      (when (grab--valid-match-p)
        (throw 'found t)))
    nil))

(defun grab--search-backward ()
  (catch 'found
    (while (re-search-backward grab--header-regexp nil :noerror)
      (when (grab--valid-match-p)
        (throw 'found t)))
    nil))

(defun grab-next-match ()
  "Move to the next match and display it."
  (interactive)
  (forward-line 1)
  (if (grab--search-forward)
      (progn
        (beginning-of-line)
        (grab-display-match))
    (forward-line -1)
    (message "No more matches")))

(defun grab-prev-match ()
  "Move to the previous match and display it."
  (interactive)
  (forward-line -1)
  (if (grab--search-backward)
      (progn
        (beginning-of-line)
        (grab-display-match))
    (forward-line 1)
    (message "No previous matches")))

(defun grab-next-error (&optional arg reset)
  "TODO"
  (interactive "p")
  (when reset
    (goto-char (point-min)))
  (let ((direction (if (< arg 0) -1 +1))
        (count (abs arg)))
    (dotimes (_ count)
      (if (> direction 0)
          (progn
            (end-of-line)
            (unless (grab--search-forward)
              (error "No more matches")))
        (beginning-of-line)
        (unless (grab--search-backward)
          (error "No previous matches"))))
    (beginning-of-line)
    (grab-goto-match)))


;;; Interactive Commands

;;;###autoload
(defun grab (pattern)
  "Run grab with PATTERN in the current directory."
  (interactive
   (list (read-string (format-prompt "Grab Pattern" nil) grab-default-pattern)))
  (grab--run-process
   (flatten-tree (list grab-command grab-command-arguments pattern))
   "*grab*"))

;;;###autoload
(defun git-grab (pattern)
  "Run git grab with PATTERN in the current directory."
  (interactive
   (list (read-string (format-prompt "Grab Pattern" nil) grab-default-pattern)))
  (grab--run-process
   (flatten-tree (list git-grab-command git-grab-command-arguments pattern))
   "*grab*"))

;;;###autoload
(defun project-grab (pattern)
  "Run grab with PATTERN at the project root."
  (interactive
   (list (read-string (format-prompt "Grab Pattern" nil) grab-default-pattern)))
  (let* ((project (project-current t))
         (default-directory (project-root project)))
    (grab--run-process
     (flatten-tree (list grab-command grab-command-arguments pattern))
     "*grab*")))

;;;###autoload
(defun project-git-grab (pattern)
  "Run git grab with PATTERN at the project root."
  (interactive
   (list (read-string (format-prompt "Grab Pattern" nil) grab-default-pattern)))
  (let* ((project (project-current t))
         (default-directory (project-root project)))
    (grab--run-process
     (flatten-tree (list git-grab-command git-grab-command-arguments pattern))
     "*grab*")))

(provide 'grab)
;;; grab.el ends here
