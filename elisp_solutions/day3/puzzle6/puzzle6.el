(with-temp-buffer
  (insert-file-contents "puzzle6_input.txt")
  (setq puzzle-lines (split-string (buffer-string) "\n" t)))

(defun find-adjacent-star (number-info perimeter-line perimeter-line-index)
  "Checks if adjacent * exists, and returns (number line-index char-index?) list."
  (setq correspondent-area (substring perimeter-line
                                      (max 0 (- (car (cadr number-info)) 1))
                                      (min (+ (cdr (cadr number-info)) 1) (length perimeter-line))))
  (setq is-at-start (= 0 (caadr number-info)))
  (setq is-at-end (= (length perimeter-line) (cdadr number-info)))
  (setq boundary-offset (if is-at-start 1 0))
  ;; (setq boundary-offset (if is-at-end -1 0))

  (setq star-index (string-match-p "*" correspondent-area))
  (if star-index
      (list (car number-info)
            (list perimeter-line-index (+
                                        star-index
                                        (caadr number-info)
                                        boundary-offset)))
    nil))

(defun find-adjacent-stars (lines)
  (setq adjacent-stars '())
  (dotimes (i (length lines))
    (setq upper-line-index (max 0 (- i 1)))
    (setq upper-line (nth upper-line-index lines))

    (setq self-line (nth i lines))

    (setq below-line-index (min (+ i 1) (length lines)))
    (setq below-line (nth below-line-index lines))

    (setq number-infos (parse-line-for-numbers self-line))
    (dolist (number-info number-infos)
      (when (not (equal upper-line-index i))
        (setq adjacent-upper-line-star (find-adjacent-star number-info upper-line upper-line-index))
        (when adjacent-upper-line-star
          (push adjacent-upper-line-star adjacent-stars)))

      (setq adjacent-self-line-star (find-adjacent-star number-info self-line i))
      (when adjacent-self-line-star
        (push adjacent-self-line-star adjacent-stars))

      (when (not (equal below-line-index i))
        (when below-line
          (setq adjacent-below-line-star (find-adjacent-star number-info below-line below-line-index))
          (when adjacent-below-line-star
            (push adjacent-below-line-star adjacent-stars)))))
    )
  adjacent-stars)

(defun find-multiplication-candidate-number (star-touching-numbers)
  "Input (Number (line charIndex)): ((598 (8 5)) (755 (8 5)) (617 (4 3)) (35 (1 3)) (467 (1 3)))"
  (let ((star-touching-numbers-map (make-hash-table :test 'equal)))
    (dolist (star-touching-number star-touching-numbers)
      (push (car star-touching-number) (gethash (cadr star-touching-number) star-touching-numbers-map)))

    ;; star-touching-numbers-map sample:  ((8 5) (755 598) (4 3) (617) (1 3) (467 35))
    ;; Iterate this map and filter values that has more than 2 elements
    ;; Write here
    (print (hash-table-keys star-touching-numbers-map))
    (print (hash-table-values star-touching-numbers-map))
    (hash-table-values star-touching-numbers-map)
    ;; star-touching-numbers-map
    (setq multiplication-candidates (seq-filter (lambda (x) (>= (length x) 2))
                                                (hash-table-values star-touching-numbers-map)))
    multiplication-candidates))

(defun parse-line-for-numbers (line)
  "Parses line '467..114..' and returns such like ((467 (0 . 2)) (114 (5 . 7)))"

  (let ((index 0) numbers)
    (while (string-match "\\([0-9]+\\)" line index)
      ;; (print (nth line (match-beginning 0)))
      (push (list
             (string-to-number (match-string 1 line))
             (cons (match-beginning 0) (match-end 0))
             )
            numbers)
      (setq index (match-end 0)))
    (nreverse numbers)))

(setq multiplication-numbers (find-multiplication-candidate-number (find-adjacent-stars puzzle-lines)))
(setq multiplied-values (mapcar (lambda (numbers) (apply '* numbers)) multiplication-numbers))
(cl-reduce '+ multiplied-values)

(concat "Answer is " (number-to-string 87449461))
