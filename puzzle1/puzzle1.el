(with-temp-buffer
  (insert-file-contents "puzzle1_input.txt")
  (setq puzzle_lines (split-string (buffer-string) "\n" t)))

(defun get-digits-only (content)
  (mapcar #'char-to-string
          (string-to-list
           (replace-regexp-in-string "[^0-9]+" "" content))))

(defun get-first-and-last-digits (content)
  (let ((digits (get-digits-only content)))
    (list (car digits) (car (last digits)))))

(defun get-correspondent-number (content)
  (string-to-number (mapconcat 'identity (get-first-and-last-digits content) "")))

(cl-reduce '+ (mapcar #'get-correspondent-number puzzle_lines))
