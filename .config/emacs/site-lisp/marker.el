(require 'hi-lock)
(require 'seq)

(defun marker-mark ()
  (interactive)
  (marker-mark-region (if (use-region-p)
                          (region-bounds)
                        `((,(pos-bol) . ,(pos-eol)))))
  (when (region-active-p)
    (deactivate-mark)))

(defun marker-mark-region (bounds)
  (dolist (x bounds) (marker--mark-region (car x) (cdr x))))

(defun marker--mark-region (beg end)
  (let ((ov (make-overlay beg end nil :front-advance)))
    (overlay-put ov 'priority 1)
    (overlay-put ov 'face 'hi-yellow)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'marker--mark-p t)))

(defun marker-unmark ()
  (interactive)
  (if (use-region-p)
      (marker-unmark-region (region-bounds))
    (marker-clear))
  (when (region-active-p)
    (deactivate-mark)))

(defun marker-unmark-region (bounds)
  (dolist (x bounds) (marker--unmark-region (car x) (cdr x))))

(defun marker--unmark-region (beg end)
  (dolist (ov (seq-filter (lambda (ov) (overlay-get ov 'marker--mark-p))
                          (overlays-in beg end)))
    (cond ((< (overlay-start ov) beg)
           (move-overlay ov (overlay-start ov) beg))
          ((> (overlay-end ov) end)
           (move-overlay ov end (overlay-end ov)))
          (:else
           (delete-overlay ov)))))

(defun marker-clear ()
  (interactive)
  (remove-overlays nil nil 'marker--mark-p t))

(provide 'marker)
