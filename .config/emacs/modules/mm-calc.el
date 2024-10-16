;;; mm-calc.el --- Emacs configurations for ‘calc-mode’  -*- lexical-binding: t; -*-

;; TODO: Swap more than 2 elements?
(defun mm-calc-swap ()
  "Swap the top two elements on the stack."
  (declare (modes calc-mode))
  (interactive)
  (calc-over 2)
  (calc-truncate-up 2)
  (calc-pop 1)
  (calc-truncate-down 2))

(use-package calc
  :bind (:map calc-mode-map
         ("C-c x" . #'mm-calc-swap))
  ;; TODO: Can this be done in :custom?
  :init
  (setopt
   calc-display-trail nil
   calc-group-digits t
   ;; Optimize for Europeans
   calc-point-char ","
   calc-group-char "."))

(provide 'mm-calc)
