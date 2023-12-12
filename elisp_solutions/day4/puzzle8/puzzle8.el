(defun parse-line (line)
  "Returns (card-no, matching-numbers)."
  (let* ((sections (split-string line ":"))
         (card-no (last (cdr (split-string (car sections) " "))))
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

(defun calculate-card-birthers (matching-line)
  "Returns card-number and birthed-copy-card-number"
  (setq card-number (car matching-line))
  (setq matchings (nth 1 matching-line))
  (setq amount-of-copy-cards (length matchings))
  (list (string-to-number card-number) amount-of-copy-cards))

(defun get-original-card-from-line (raw-line)
  (calculate-card-birthers
   (get-matching-numbers-in-a-line
    (parse-line raw-line))))

(defun get-original-cards (input-txt-data)
  (with-temp-buffer
    (insert-file-contents input-txt-data)
    (setq puzzle-lines (split-string (buffer-string) "\n" t))
    (setq original-cards (mapcar #'get-original-card-from-line puzzle-lines))
    original-cards))

(defun main ()
  (setq original-cards (get-original-cards "puzzle8_input.txt"))

  (setq original-cards-to-sort (copy-sequence original-cards))
  (setq sorted-original-cards (sort original-cards-to-sort (lambda (a b) (< (cadr a) (cadr b)))))
  (setq cards (make-hash-table :test 'equal))

  ;; Pseudo code
  ;; result-of-numbers
  ;; if numbers length is 0 -> 0

  ;; if numbers length is 1 ->
    ;;; if value is 0 -> 1
    ;;; if value is (n) -> (cached) or (+ 1 (result-of-numbers (subseq)))

  ;; if numbers length is n -> ('+ (result-of-numbers (list (car numbers)) (cdr numbers)))

  (setq result-of-numbers-cache (make-hash-table :test 'equal))

  (defun result-of-numbers (numbers)
    (cl-case (length numbers)
      (0 0)
      (1 (let ((number (car numbers)))
           (setq processing-card-no (car number))
           (setq processing-card-value (cadr number))
           (setq cache-value (gethash (car number) result-of-numbers-cache))
           (if cache-value
               cache-value
             (progn
               (setq birth-seq (cl-subseq original-cards processing-card-no (+ processing-card-no processing-card-value)))
               (setq result (+ 1 (result-of-numbers birth-seq)))
               (puthash (car number) result result-of-numbers-cache)
               result
               )
             )
           ))
      (t (+ (result-of-numbers (list (car numbers)))
            (result-of-numbers (cdr numbers))))))

  (result-of-numbers sorted-original-cards))

;; Answer was 10212704
(main)
