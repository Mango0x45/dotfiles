;;; increment.el -- Increment numbers at point  -*- lexical-binding: t; -*-

(defun increment--number-to-binary-string (number)
  (let (s)
    (while (not (= number 0))
      (setq s (concat (if (= 1 (logand number 1)) "1" "0") s)
            number (lsh number -1)))
    (when (string= s "")
      (setq s "0"))
    s))

(defun increment--format-number-with-base
    (number base leading-zeros buffer-substr hex-style)
  (let* ((neg (> 0 number))
         (number (abs number))
         (number-string
          (cond ((= base  2) (increment--number-to-binary-string number))
                ((= base  8) (format "%o" number))
                ((= base 10) (number-to-string number))
                ((= base 16) (format (if (eq hex-style 'lower)
                                         "%x"
                                       "%X")
                                     number))
                ((t (error (format "Invalid base %d") base)))))
         (length-diff (- (length buffer-substr)
                         (length number-string)))
         (leading-zeros (if (> leading-zeros 0)
                            (+ leading-zeros length-diff)
                          0)))
    (concat
     (when neg
       "-")
     (cond ((= base  2) "0b")
           ((= base  8) "0o")
           ((= base 16) "0x"))
     (when (> leading-zeros 0)
       (make-string leading-zeros ?0))
     number-string)))

;;;###autoload
(defun increment-number-at-point (&optional arg)
  "Increment the number at point by ARG or 1 if ARG is nil.  If called
interactively, the universal argument can be used to specify ARG.  If
the number at point has leading zeros then the width of the number is
preserved."
  (interactive "*p")
  (save-match-data
    (let (hex-style case-fold-search)
      (when-let* ((base
                   (cond
                    ((thing-at-point-looking-at
                      "\\(-\\|\\b\\)0x\\(0*\\)\\([0-9a-f]+\\)\\b")
                     (setq hex-style 'lower)
                     16)
                    ((thing-at-point-looking-at
                      "\\(-\\|\\b\\)0x\\(0*\\)\\([0-9A-Fa-f]+\\)\\b")
                     16)
                    ((thing-at-point-looking-at
                      "\\(-?\\)\\(0*\\)\\([0-9]+\\)")
                     10)
                    ((thing-at-point-looking-at
                      "\\(-\\|\\b\\)0o\\(0*\\)\\([0-7]+\\)\\b")
                     8)
                    ((thing-at-point-looking-at
                      "\\(-\\|\\b\\)0b\\(0*\\)\\([01]+\\)\\b")
                     2)))
                  (substr (buffer-substring-no-properties
                           (match-beginning 3) (match-end 3)))
                  (sign (if (= (match-beginning 1) (match-end 1)) +1 -1))
                  (result (+ (* (string-to-number substr base) sign)
                             (or arg 1))))
        (replace-match (increment--format-number-with-base
                        result base
                        (- (match-end 2)
                           (match-beginning 2))
                        substr hex-style))))))

;;;###autoload
(defun decrement-number-at-point (&optional arg)
  "The same as `increment-number-at-point', but ARG is negated."
  (interactive "*p")
  (increment-number-at-point (- (or arg 1))))

(provide 'increment)
