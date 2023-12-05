(with-temp-buffer
  (insert-file-contents "puzzle5_input.txt")
  (setq puzzle-lines (split-string (buffer-string) "\n" t)))

(defun parse-line-for-numbers (line)
  "Parses line '467..114..' and returns such like ((467 (0 . 2)) (114 (5 . 7)))"

  (let ((index 0) numbers)
    (while (string-match "\\([0-9]+\\)" line index)
      (push (list (string-to-number (match-string 1 line))
                  (cons (match-beginning 0)
                        (match-end 0)))
            numbers)
      (setq index (match-end 0)))
    (nreverse numbers)))

(defun is-perimeter-good-for-number (number-info perimeter-line)
  "Checks if adjacent symbol exists if so return part of engine."
  (setq correspondent-area (substring perimeter-line
                                      (max 0 (- (car (cadr number-info)) 1))
                                      (min (+ (cdr (cadr number-info)) 1) (length perimeter-line))
                                      ))
  ;; (print correspondent-area)
  ;; Check if any char exists other than digit or '.'
  (if (string-match-p "[^0-9.]" correspondent-area)
      t
    nil))

(defun is-number-part-of-engine (number-info perimeter-lines)
  (catch 'found
    (dolist (line perimeter-lines)
      (when line
        (when (is-perimeter-good-for-number number-info line)
          (throw 'found t))))
    nil))

(defun get-engine-numbers-from-lines (lines)
  (setq engine-numbers '())
  (dotimes (i (length lines))
    (setq upper-line (nth (max 0 (- i 1)) lines))
    (setq self-line (nth i lines))
    (setq below-line (nth (min (+ i 1) (length lines)) lines))
    (setq perimeter-lines (list upper-line self-line below-line))
    (setq number-infos (parse-line-for-numbers self-line))

    (dolist (number-info number-infos)
      (when (is-number-part-of-engine number-info perimeter-lines)
        (push number-info engine-numbers))))
  engine-numbers)

(defvar engine-number-infos (get-engine-numbers-from-lines puzzle-lines))

(defvar engine-values (mapcar #'car engine-number-infos))

(cl-reduce '+ engine-values)
