;;; mm-documentation.el --- Configuration related to documentation  -*- lexical-binding: t; -*-

;;; Enhance Describe Commands

(use-package helpful
  :ensure t
  :bind (([remap describe-command]  . helpful-command)
         ([remap describe-function] . helpful-callable)
         ([remap describe-key]      . helpful-key)
         ([remap describe-symbol]   . helpful-symbol)
         ([remap describe-variable] . helpful-variable)
         (("C-h C-p" . helpful-at-point))))


;;; Open Manpage for Symbol

(defun mm-documentation-man-at-point ()
  "Open a UNIX manual page for the symbol at point."
  (declare (modes (c-mode c++-mode c-ts-mode c++-ts-mode)))
  (interactive)
  (if-let ((symbol
            (pcase major-mode
              ((or 'c-mode 'c++-mode)
               (thing-at-point 'symbol :no-properties))
              ((or 'c-ts-mode 'c++-ts-mode)
               (when-let ((node (treesit-thing-at-point "identifier" 'nested)))
                 (treesit-node-text node :no-properties))))))
      (man symbol)
    (message "No symbol at point.")))


;;; Browse RFC Pages

(use-package rfc-mode
  :ensure t
  :custom
  (rfc-mode-directory (expand-file-name "rfc" (xdg-user-dir "DOCUMENTS")))
  :config
  (unless (featurep 'consult)
    (keymap-set rfc-mode-map "g" #'imenu))
  (with-eval-after-load 'consult
    (keymap-set rfc-mode-map "g" #'consult-imenu)))

(provide 'mm-documentation)