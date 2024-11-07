;;; mm-modeline.el --- Pluggable modeline components  -*- lexical-binding: t; -*-

(defmacro mm-modeline--define-component (name &rest forms)
  (declare (indent 1))
  `(progn
     (defface ,(intern (format "%s-face" name))
       '((t))
       ,(format "Face for the `%s' component." name))
     (defvar-local ,name '(:eval (or ,(macroexp-progn forms) "")))
     (put ',name 'risky-local-variable t)))


;;; Support Icons

(use-package all-the-icons
  :ensure t
  :init
  (defvar mm-all-the-icons-cookie
    (expand-file-name ".all-the-icons-installed-p" mm-cache-directory))
  (unless (file-exists-p mm-all-the-icons-cookie)
    (all-the-icons-install-fonts)
    (make-empty-file mm-all-the-icons-cookie :parents))
  (set-char-table-range char-width-table #xE907 2))


;;; Modeline Components

(mm-modeline--define-component mm-modeline-readonly
  (when buffer-read-only
    (propertize " READONLY" 'face 'mm-modeline-readonly-face)))

(mm-modeline--define-component mm-modeline-buffer-name
  (propertize "%b" 'face 'mm-modeline-buffer-name-face))

(mm-modeline--define-component mm-modeline-buffer-modified
  (when (and (buffer-modified-p)
             (buffer-file-name))
    (propertize " (modified)" 'face 'mm-modeline-buffer-modified-face)))

(defconst mm-modeline-mode-acronyms
  '("css" "csv" "gsp" "html" "json" "mhtml" "scss" "toml" "tsv")
  "TODO")

(defconst mm-modeline-remap-alist
  '(("Js"      . "JavaScript")
    ("Ts Mode" . "Tree-Sitter Mode")
    ("Gmake"   . "GMake")
    ("Imake"   . "IMake")
    ("Bsdmake" . "BSD Make"))
  "TODO")

(mm-modeline--define-component mm-modeline-major-mode-name
  (propertize
   (let ((string (thread-last
                   major-mode
                   (symbol-name)
                   (capitalize)
                   (string-replace "-" " ")))
         (case-fold-search nil))
     ;; TODO: Do I need ‘save-match-data’
     (save-match-data
       (dolist (pair mm-modeline-remap-alist)
         (setq string
               (replace-regexp-in-string
                (format "\\<%s\\>" (regexp-quote (car pair)))
                (cdr pair) string)))
       (setq case-fold-search t)
       (if (string-match (regexp-opt mm-modeline-mode-acronyms 'words) string)
           (concat
            (substring string 0 (match-beginning 0))
            (upcase (substring string (match-beginning 0) (match-end 0)))
            (substring string (match-end 0) (length string)))
         string)))
   'face 'mm-modeline-major-mode-name-face))

(mm-modeline--define-component mm-modeline-major-mode-symbol
  (propertize
   (cond
    ((derived-mode-p 'comint-mode)  "$ ")
    ((derived-mode-p 'conf-mode)    "# ")
    ((derived-mode-p 'prog-mode)    "λ ")
    ((derived-mode-p 'special-mode) "❇ ")
    ((derived-mode-p 'text-mode)    "§ ")
    (:default ""))
   'face 'mm-modeline-major-mode-symbol-face))

(mm-modeline--define-component mm-modeline-narrow
  (when (buffer-narrowed-p)
    (propertize
     " Narrow "
     'face 'mm-modeline-narrow-face)))

(mm-modeline--define-component mm-modeline-git-branch
  (when-let ((branch (car (and (featurep 'vc-git)
                               (vc-git-branches)))))
    (concat
     (propertize "\uE907" 'display '(raise 0))
     " "
     (propertize branch 'face 'mm-modeline-git-branch-face)
     " │ ")))


;;; Padding Between Left and Right

(mm-modeline--define-component mm-modeline-left-right-padding
  (let ((length (string-width (format-mode-line mm-modeline-right))))
    (propertize " " 'display `(space :align-to (- right ,length)))))


;;; Configure Modeline

(setopt mode-line-format-right-align 'right-margin)

(setq
 mm-modeline-left (list mm-modeline-narrow
                        mm-modeline-readonly
                        " "
                        mm-modeline-buffer-name
                        mm-modeline-buffer-modified
                        " │ "
                        mm-modeline-major-mode-symbol
                        mm-modeline-major-mode-name
                        mm-modeline-left-right-padding
                        mode-line-end-spaces)
 mm-modeline-right (list mm-modeline-git-branch
                         "%l:%c "))

(setq-default
 mode-line-format
 (list mm-modeline-left mm-modeline-left-right-padding mm-modeline-right))
(provide 'mm-modeline)