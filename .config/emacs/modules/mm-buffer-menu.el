;;; mm-buffer-menu.el --- Buffer Menu configuration  -*- lexical-binding: t; -*-

(defun mm-buffer-menu-delete-all ()
  "Mark all buffers for deletion."
  (interactive nil Buffer-menu-mode)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (Buffer-menu-delete))))

(use-package buff-menu
  :bind ( :map Buffer-menu-mode-map
          ("D" . mm-buffer-menu-delete-all)))

(provide 'mm-buffer-menu)
