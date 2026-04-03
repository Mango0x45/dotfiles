;;; tree-exp-mode.el --- Minor mode for expanding tree-like syntax  -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-macs)
  (require 'tempo))

(defgroup tree-exp nil
  "Customization group for `emmet-mode'."
  :group 'convenience)

(defcustom tree-exp-html-self-closing-style ""
  "Self-closing tag style for HTML.
This setting specifies how `tree-exp' should generate self-closing tags
when expanding to HTML.  When generating a self-closing tag the value of
`tree-exp-html-self-closing-style' is inserted verbatim between the end
of the element attributes and the closing angle-bracket.

Note that the variable of this setting has no bearing on the validity of
the generated HTML; infact the HTML standard instructs parsers to ignore
a trailing slash."
  :type '(choice (const :tag "")
                 (const :tag "/")
                 (const :tag " /"))
  :package-version '(tree-exp . "1.0.0")
  :group 'tree-exp)

(defcustom tree-exp-set-marks t
  "TODO"
  :type 'boolean
  :package-version '(tree-exp . "1.0.0")
  :group 'tree-exp)

(defvar-keymap tree-exp-mode-map
  :doc "TODO")

(defvar tree-exp-expand-functions
  '(((html-mode mhtml-mode html-ts-mode) . tree-exp-expand-html)
    (gsp-ts-mode . tree-exp-expand-gsp))
  "TODO")

(defvar tree-exp-operator-alist-alist
  '(((html-mode mhtml-mode html-ts-mode gsp-ts-mode)
     . ((?+ . sibling)
        (?^ . parent)
        (?> . child)
        (?. . extra)
        (?# . extra)
        (?@ . extra)
        (?* . repeat))))
  "TODO")

(defvar tree-exp-after-expansion-hook nil
  "TODO")


;; Private Variables

(defvar-local tree-exp--mark-index 0)

(defconst tree-exp--html-void-elements
  #s(hash-table size 13
                test equal
                purecopy t
                data ("area"   t
                      "base"   t
                      "br"     t
                      "col"    t
                      "embed"  t
                      "hr"     t
                      "img"    t
                      "input"  t
                      "link"   t
                      "meta"   t
                      "source" t
                      "track"  t
                      "wbr"    t)))


;; Parsing

(defun tree-exp--pattern-start ()
  (max (save-excursion
         (skip-syntax-backward "^ ")
         (point))
       (pos-bol)))

(defun tree-exp--pattern-bounds ()
  (if (use-region-p)
      (progn
        (when (region-noncontiguous-p)
          (error "Noncontiguous regions are not supported yet."))
        (car (region-bounds)))
    (cons (tree-exp--pattern-start) (point))))

(defun tree-exp--symbol-end (operator-alist)
  (while (and (memq (car (syntax-after (point))) '(2 3))
              (not (assq (char-after (point)) operator-alist)))
    (goto-char (1+ (point))))
  (point))

(defun tree-exp--set-children-of-leaves (tree values)
  (dolist (node tree)
    (if-let ((children (nth 2 node)))
        (tree-exp--set-children-of-leaves children values)
      (setf (nth 2 node) values)))
  tree)

(defun tree-exp--append-extra-to-leaves (tree value)
  (dolist (node tree)
    (if-let ((children (nth 2 node)))
        (tree-exp--append-extra-to-leaves children value)
      (setf
       (nth 1 node)
       (append (nth 1 node) (cons value nil)))))
  tree)

(defun tree-exp--parse-count ()
  (let* ((start (point))
         (end (+ start (skip-chars-forward "0-9")))
         (num (string-to-number
               (buffer-substring-no-properties start end)
               10)))
    (unless (zerop num)
      num)))

(defun tree-exp--parse-atom (operator-alist)
  (let ((start (point))
        (end (tree-exp--symbol-end operator-alist)))
    (if (= start end)
        (when (= ?\( (char-after end))
          (goto-char (1+ end))
          (let ((tree (tree-exp--parse-expr operator-alist))
                (char (char-after (point))))
            (if (eq ?\) char)
                (progn
                  (goto-char (1+ (point)))
                  tree)
              (message (if char
                           (format "Invalid operator `%c'" char)
                         "Missing closing parenthesis"))
              nil)))
      (list (list (buffer-substring-no-properties start end) nil nil)))))

(defun tree-exp--parse-term (operator-alist)
  (let ((terms (tree-exp--parse-atom operator-alist)))
    (catch 'loop
      (while t
        (let* ((operator (assq (char-after (point)) operator-alist))
               (op-char (car-safe operator))
               (op-type (cdr-safe operator))
               start end)
          (unless (eq op-type 'extra)
            (throw 'loop terms))
          (setq
           start (goto-char (1+ (point)))
           end (tree-exp--symbol-end operator-alist))
          (when (= start end)
            (message "Operator `%c' missing right-hand side" op-char)
            (throw 'loop nil))
          (tree-exp--append-extra-to-leaves
           terms (cons op-char (buffer-substring-no-properties start end))))))))

;; Gross hack
(defun tree-exp--parse-expr-with-lhs (operator-alist lhs)
  (let* ((op-char (char-after (point)))
         (op-type (alist-get op-char operator-alist))
         rhs)
    (cond
     ((not lhs)
      nil)
     ((or (not op-char)
          (not op-type))
      lhs)
     (t
      (goto-char (1+ (point)))
      (setq rhs (if (eq op-type 'repeat)
                    (tree-exp--parse-count)
                  (tree-exp--parse-expr operator-alist)))
      (when rhs
        (pcase op-type
          ('child  (tree-exp--set-children-of-leaves lhs rhs))
          ('parent (tree-exp--set-children-of-leaves rhs lhs))
          ('sibling (append lhs rhs))
          ('repeat (tree-exp--parse-expr-with-lhs
                    operator-alist
                    ;; FIXME: We need to call ‘copy-tree’ or else the
                    ;; generated AST has cycles in it… why is that?  How
                    ;; can we fix it?
                    (cl-loop for _ from 1 to rhs append (copy-tree lhs))))))))))

(defun tree-exp--parse-expr (operator-alist)
  (tree-exp--parse-expr-with-lhs
   operator-alist
   (tree-exp--parse-term operator-alist)))

(defun tree-exp--build-ast (bounds)
  (save-excursion
    (with-restriction (car bounds) (cdr bounds)
      (goto-char 1)
      (when-let* ((operator-alist (tree-exp--alist-get
                                   major-mode tree-exp-operator-alist-alist))
                  (tree (tree-exp--parse-expr operator-alist)))
        (if (= (point) (point-max))
            tree
          (message "Superfluous character `%c'" (char-after (point)))
          nil)))))


;; Expansion

(defun tree-exp--alist-get (key alist)
  (cdr (or (assq key alist)
           (catch 'tree-exp--break
             (dolist (pair alist)
               (when (and (listp (car pair))
                          (memq key (car pair)))
                 (throw 'tree-exp--break pair)))))))

(defun tree-exp--ast-siblings-p (ast)
  (cond ((not ast) nil)
        ((length> ast 1) t)
        (t (tree-exp--ast-siblings-p (caddar ast)))))

(defun tree-exp-expand-html--format-attr (attr)
  (declare ((pure t) (side-effect-free t)))
  (let* ((parts (string-split attr "="))
         (name        (car parts))
         (value-parts (cdr parts)))
    (if value-parts
        (format "%s=\"%s\"" name (string-join value-parts "="))
      name)))

(defun tree-exp-expand-html--helper (ast indentp)
  (dolist (node ast)
    (let* ((name     (nth 0 node))
           (attrs    (nth 1 node))
           (children (nth 2 node))
           (classes (cl-loop for (op . attr) in attrs
                             if (= ?. op)
                             collect attr)))
      (insert (format "<%s" name))
      (when classes
        (insert (format " class=\"%s\"" (string-join classes " "))))
      (cl-loop for (op . attr) in attrs
               if (= ?# op)
                 do (insert (format " id=\"%s\"" attr))
               else if (= ?@ op)
                 do (thread-last
                      attr
                      (tree-exp-expand-html--format-attr)
                      (concat " ")
                      (insert)))
      (if (and (not children)
               (gethash name tree-exp--html-void-elements))
          (insert (format "%s>" tree-exp-html-self-closing-style))
        (insert ?>)
        (if children
            (progn
              (when indentp
                (insert ?\n))
              (tree-exp-expand-html--helper children indentp))
          (when tree-exp-set-marks
            (insert #x1B)))
        (insert (format "</%s>" name))))
    (when indentp
      (insert ?\n))))

(defun tree-exp-expand-html (ast)
  "TODO"
  (tree-exp-expand-html--helper ast (tree-exp--ast-siblings-p ast))
  (when (= ?\n (char-before (point-max)))
    (delete-region (1- (point-max)) (point-max)))
  (when tree-exp-set-marks
    (insert #x1B)))

;; (defun tree-exp-expand-gsp--helper (ast indentp)
;;   (dolist (node ast)
;;     (let ((name     (nth 0 node))
;;           (attrs    (nth 1 node))
;;           (children (nth 2 node)))


;;       ;; (insert (format "<%s" name))
;;       ;; (dolist (attr attrs)
;;       ;;   (insert (format " %s" (cdr attr))))
;;       ;; (if (and (not children)
;;       ;;          (gethash name tree-exp--html-void-elements))
;;       ;;     (insert (format "%s>" tree-exp-html-self-closing-style))
;;       ;;   (insert ?>)
;;       ;;     (if children
;;       ;;         (progn
;;       ;;           (when indentp
;;       ;;             (insert ?\n))
;;       ;;           (tree-exp-expand-html--helper children indentp))
;;       ;;       (when tree-exp-set-marks
;;       ;;         (insert #x1B)))
;;       ;;     (insert (format "</%s>" name)))
;;       )
;;     (when indentp
;;       (insert ?\n))))

(defun tree-exp-expand-gsp (ast)
  "TODO"
  (tree-exp-expand-gsp--helper ast (tree-exp--ast-siblings-p ast))
  (when (= ?\n (char-before (point-max)))
    (delete-region (1- (point-max)) (point-max)))
  (when tree-exp-set-marks
    (insert #x1B)))

;;;###autoload
(defun tree-exp-expand ()
  "TODO"
  (interactive)
  (when-let* ((current-buffer (current-buffer))
              (bounds (tree-exp--pattern-bounds))
              (ast (tree-exp--build-ast bounds))
              (expander (tree-exp--alist-get
                         major-mode tree-exp-expand-functions)))
    (delete-region (car bounds) (cdr bounds))
    (with-temp-buffer
      (funcall expander ast)
      (insert-into-buffer current-buffer))
    (let ((start-pos (car bounds))
          (end-pos (point-marker))
          (delete-active-region nil)    ; For ‘delete-backward-char’
          marks)
      (when tree-exp-set-marks
        (save-excursion
          (goto-char start-pos)
          (while (search-forward "\x1B" end-pos :noerror)
            (delete-backward-char 1)
            (push (point-marker) marks)))
        (push (point-marker) marks)
        (setq marks (nreverse marks))
        (goto-char (car marks))
        (mapc #'tempo-insert-mark marks))
      ;; Set the start of the region to the start bound
      (unless (region-active-p)
        (set-mark start-pos))
      (indent-region start-pos end-pos)
      (run-hooks tree-exp-after-expansion-hook))))

;;;###autoload
(define-minor-mode tree-exp-mode
  "TODO"
  :lighter " Tree-Expand"
  :keymap tree-exp-mode-map)

(provide 'tree-exp)
