;; Parse a line
;; Solve for 1 line
;;   Find elements intersected in a set.
;;   Return all intersected element set
;;   Calculate the value of the first card 2 ^ (length - 1)

(defun parse-line (line)
  "Returns (card-no, matching-numbers)."
  (let* ((sections (split-string line ":"))
         (card-no (cdr (split-string (car sections) " ")))
         (numbers (cdr sections)))
    sections
    (setq numbers-pair (split-string (car numbers) "|"))
    (setq winning-numbers (split-string (car numbers-pair) " "))
    (setq numbers-in-hand (split-string (cadr numbers-pair) " "))
    (setq parsed-line (list (car card-no) (remove "" winning-numbers) (remove "" numbers-in-hand)))
    parsed-line))

(defun get-matching-numbers-in-a-line (parsed-line)
  "Returns matching numbers with a card no (1, (list matching numbers))"
  (setq matchings (cl-intersection (nth 1 parsed-line) (nth 2 parsed-line) :test 'equal))
  (list (car parsed-line) matchings))

(defun calculate-card-prize (matching-line)
  "Returns prize 2 ^ lenth - 1"
  (setq matchings (nth 1 matching-line))
  (setq prize (expt 2 (- (length matchings) 1)))
  (if (>= prize 1) prize 0))

(defun calculate-card-prize-from-raw-line (raw-line)
  (calculate-card-prize
   (get-matching-numbers-in-a-line
    (parse-line raw-line))))

(defun calculate-total-prize (input-txt-data)
  (with-temp-buffer
    (insert-file-contents input-txt-data)
    (setq puzzle-lines (split-string (buffer-string) "\n" t))
    (setq prizes (mapcar #'calculate-card-prize-from-raw-line puzzle-lines))
    (cl-reduce '+ prizes)))

(defun main ()
  ;; (calculate-total-prize "puzzle7_input_sample.txt")
  (calculate-total-prize "puzzle7_input.txt"))

;; Answer is: 28750
(main)
