;;; mm-tetris.el --- Emacs configurations for ‘tetris’  -*- lexical-binding: t; -*-

(defun mm-tetris-rotate-mirror ()
  "Rotate the current piece by 180°."
  (interactive nil tetris-mode)
  (tetris-rotate-next)
  (tetris-rotate-next))

(use-package tetris
  :bind ( :map tetris-mode-map
          ("a"   . tetris-move-left)
          ("d"   . tetris-move-right)
          ("k"   . tetris-rotate-next)
          (";"   . tetris-rotate-prev)
          ("l"   . tetris-move-down)
          ("o"   . mm-tetris-rotate-mirror)
          ("SPC" . tetris-move-bottom)))

(provide 'mm-tetris)
