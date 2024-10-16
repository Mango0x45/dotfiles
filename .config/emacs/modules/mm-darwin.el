;;; mm-darwin.el --- MacOS Configuration  -*- lexical-binding: t; -*-

(unless (featurep 'ns)
  (error "'NS not available.  Something has gone horribly wrong."))


;;; Launch Emacs Properly

(defun mm-ns-raise-emacs ()
  (ns-do-applescript "tell application \"Emacs\" to activate"))

(add-hook
 'after-make-frame-functions
 (defun mm-ns-raise-emacs-with-frame (frame)
   (when (display-graphic-p)
     (with-selected-frame frame
       (mm-ns-raise-emacs)))))

(when (display-graphic-p)
  (mm-ns-raise-emacs))

(provide 'mm-darwin)