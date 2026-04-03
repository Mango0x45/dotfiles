;;; mm-darwin.el --- MacOS Configuration  -*- lexical-binding: t; -*-

(unless (featurep 'ns)
  (error "'NS not available.  Something has gone horribly wrong."))


;;; Launch Emacs Properly

(defun mm-darwin--ns-raise-emacs ()
  (ns-do-applescript "tell application \"Emacs\" to activate"))

(add-hook
 'after-make-frame-functions
 (defun mm-darwin--ns-raise-emacs-with-frame (frame)
   (when (display-graphic-p)
     (with-selected-frame frame
       (mm-darwin--ns-raise-emacs)))))

(when (display-graphic-p)
  (mm-darwin--ns-raise-emacs))


;;; Set Modifier Keys

(setopt mac-option-key-is-meta t
        mac-command-key-is-meta nil)
(setopt mac-option-modifier 'meta
        mac-command-modifier 'none)

(provide 'mm-darwin)
