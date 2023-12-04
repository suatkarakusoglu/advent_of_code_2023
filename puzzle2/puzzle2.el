(with-temp-buffer
  (insert-file-contents "puzzle2_input.txt")
  (setq puzzle_lines (split-string (buffer-string) "\n" t)))

(defvar written-numbers
  '(("one" . "1")
    ("two" . "2")
    ("three" . "3")
    ("four" . "4")
    ("five" . "5")
    ("six" . "6")
    ("seven" . "7")
    ("eight" . "8")
    ("nine" . "9")))

(defun get-digits (line-content)
  (let* ((numbers '())
         (i 0))
    (while (< i (length line-content))
      (dolist (pair written-numbers)
        (when (string-prefix-p (cdr pair) (substring line-content i))
          (push (cdr pair) numbers))
        (when (string-prefix-p (car pair) (substring line-content i))
          (push (cdr pair) numbers)))
      (setq i (+ 1 i)))
    numbers))

(defun get-first-and-last-digits (content)
  (let ((digits (nreverse (get-digits content))))
    (list (car digits) (car (last digits)))))

(defun get-correspondent-number (content)
  (string-to-number (mapconcat 'identity (get-first-and-last-digits content) "")))

(cl-reduce '+ (mapcar #'get-correspondent-number puzzle_lines))
