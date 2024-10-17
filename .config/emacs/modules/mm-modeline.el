;;; mm-modeline.el --- Pluggable modeline components  -*- lexical-binding: t; -*-

(defmacro mm-modeline--define-component (name &rest forms)
  (declare (indent 1))
  `(progn
     (defvar-local ,name '(:eval (or ,(macroexp-progn forms) "")))
     (put ',name 'risky-local-variable t)))


;;; Faces

(defface mm-modeline-narrow-face
  '((t :foreground "#C5C8C6"		; From ‘mango-theme’
       :background "dark red"
       :box "dark red"
       :weight bold))
  "Face for the `mm-modeline-narrow' modeline component.")


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
    (propertize " READONLY" 'face 'bold)))

(mm-modeline--define-component mm-modeline-buffer-name
  (propertize "%b" 'face 'font-lock-constant-face))

(mm-modeline--define-component mm-modeline-buffer-modified
  (when (and (buffer-modified-p)
	     (buffer-file-name))
    (propertize " (modified)" 'face 'shadow)))

(defconst mm-modeline-mode-acronyms
  '("css" "csv" "gsp" "html" "json" "mhtml" "scss" "toml" "tsv")
  "TODO")

(mm-modeline--define-component mm-modeline-major-mode-name
  (propertize
   (let ((string
          (thread-last
            major-mode
            (symbol-name)
            (capitalize)
            (string-replace "-" " ")
            ;; TODO: Make a remap alist
            (replace-regexp-in-string "\\<Js\\>" "JavaScript")
            (string-replace "Ts Mode" "Tree-Sitter Mode")))
         (case-fold-search t))
     ;; TODO: Do I need ‘save-match-data’
     (save-match-data
       (if (string-match
            (concat "\\<" (regexp-opt mm-modeline-mode-acronyms) "\\>") string)
           (concat
            (substring string 0 (match-beginning 0))
            (upcase (substring string (match-beginning 0) (match-end 0)))
            (substring string (match-end 0) (length string)))
         string)))
   'face 'bold))

(mm-modeline--define-component mm-modeline-major-mode-symbol
  (propertize
   (cond
    ((derived-mode-p 'prog-mode)             "λ ")
    ((derived-mode-p '(text-mode conf-mode)) "§ ")
    ((derived-mode-p 'comint-mode)           ">_ ")
    (t ""))
   'face 'shadow))

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
     (propertize branch 'face 'font-lock-constant-face)
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
