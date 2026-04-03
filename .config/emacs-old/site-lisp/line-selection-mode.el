;;; line-selection-mode.el --- Minor mode for selection by lines  -*- lexical-binding: t; -*-

(defvar-local line-selection-mode--cursor-type nil)

;;;###autoload
(define-minor-mode line-selection-mode
  "Enable `hl-line-mode' and hide the current cursor."
  :global nil
  :init-value nil
  (if line-selection-mode
      (progn
        (hl-line-mode)
        (setq line-selection-mode--cursor-type cursor-type)
        (setq-local cursor-type nil))
    (hl-line-mode -1)
    (setq-local cursor-type line-selection-mode--cursor-type)))

(provide 'line-selection-mode)
