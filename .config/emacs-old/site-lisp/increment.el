;;; increment.el -- Increment numbers at point  -*- lexical-binding: t; -*-

(require 'cl-macs)
(require 'rx)

(defvar increment--binary-number-regexp
  (rx (group (or ?- word-start))
      "0b"
      (group (* ?0))
      (group (+ (any "01")))
      word-end))

(defvar increment--octal-number-regexp
  (rx (group (or ?- word-start))
      "0o"
      (group (* ?0))
      (group (+ (any "0-7")))
      word-end))

(defvar increment--decimal-number-regexp
  (rx (group (? ?-))
      (group (* ?0))
      (group (+ (any digit)))))

(defvar increment--hexadecimal-number-regexp
  (rx (group (or ?- word-start))
      "0x"
      (group (* ?0))
      (group (+ (any hex-digit)))
      word-end))

(defvar increment--hexadecimal-lower-number-regexp
  (rx (group (or ?- word-start))
      "0o"
      (group (* ?0))
      (group (+ (any "0-9a-f")))
      word-end))

(defvar increment--number-regexp
  (rx (or (seq (or ?- word-start)
               (or (seq "0b" (+ (any "01")))
                   (seq "0o" (+ (any "0-7")))
                   (seq "0x" (+ (any hex-digit))))
               word-end)
          (seq (? ?-) (+ (any digit))))))

(defun increment--number-to-binary-string (number)
  (nreverse
   (cl-loop for x = number then (ash x -1)
            while (not (= x 0))
            concat (if (= 0 (logand x 1)) "0" "1"))))

(defun increment--format-number-with-base
    (number base leading-zeros buffer-substr hex-style)
  (let* ((neg (> 0 number))
         (number (abs number))
         (number-string
          (pcase base
            (2  (increment--number-to-binary-string number))
            (8  (format "%o" number))
            (10 (number-to-string number))
            (16 (format (if (eq hex-style 'lower) "%x" "%X") number))))
         (length-diff (- (length buffer-substr)
                         (length number-string)))
         (leading-zeros (if (> leading-zeros 0)
                            (+ leading-zeros length-diff)
                          0)))
    (concat
     (when neg
       "-")
     (pcase base
       (2 "0b")
       (8 "0o")
       (16 "0x"))
     (when (> leading-zeros 0)
       (make-string leading-zeros ?0))
     number-string)))

(defun increment--match-number-at-point ()
  (cond ((thing-at-point-looking-at
          increment--binary-number-regexp)
         (cons 2 nil))
        ((thing-at-point-looking-at
          increment--octal-number-regexp)
         (cons 8 nil))
        ((thing-at-point-looking-at
          increment--hexadecimal-number-regexp)
         (cons 16 nil))
        ((thing-at-point-looking-at
          increment--hexadecimal-lower-number-regexp)
         (cons 16 'lower))
        ((thing-at-point-looking-at
          increment--decimal-number-regexp)
         (cons 10 nil))))

;;;###autoload
(cl-defun increment-number-at-point (&optional arg)
  "Increment the number at point by ARG or 1 if ARG is nil.  If called
interactively, the universal argument can be used to specify ARG.  If
the number at point has leading zeros then the width of the number is
preserved."
  (interactive "*p")
  (save-match-data
    (let (case-fold-search
          (match-pair (increment--match-number-at-point)))
      (unless match-pair
        (let ((save-point (point)))
          (unless (re-search-forward
                   increment--number-regexp
                   (line-end-position) :noerror)
            (goto-char save-point)
            (cl-return-from increment-number-at-point))
          (setq match-pair (increment--match-number-at-point))))
      (let* ((base      (car match-pair))
             (hex-style (cdr match-pair))
             (substr (buffer-substring-no-properties
                      (match-beginning 3) (match-end 3)))
             (sign (if (= (match-beginning 1) (match-end 1)) +1 -1))
             (result (+ (* (string-to-number substr base) sign)
                        (or arg 1))))
        (replace-match
         (increment--format-number-with-base
          result base (- (match-end 2) (match-beginning 2))
          substr hex-style))))))

;;;###autoload
(defun decrement-number-at-point (&optional arg)
  "The same as `increment-number-at-point', but ARG is negated."
  (interactive "*p")
  (increment-number-at-point (- (or arg 1))))

(provide 'increment)
