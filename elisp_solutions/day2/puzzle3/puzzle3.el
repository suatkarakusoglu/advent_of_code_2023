(with-temp-buffer
  (insert-file-contents "puzzle3_input.txt")
  (setq puzzle-lines (split-string (buffer-string) "\n" t)))

(defun is-game-hand-possible (game-hand red-cubes-amount green-cubes-amount blue-cubes-amount)
  (setq hand-colors (split-string game-hand ", "))
  (setq result t)
  (dolist (hand-color hand-colors)
    (setq color-info (split-string hand-color))
    (setq color-amount (string-to-number (car color-info)))
    (setq color (car (cdr color-info)))
    (cond ((string= color "red")
           (when (< red-cubes-amount color-amount)
             (setq result nil)))
          ((string= color "green")
           (when (< green-cubes-amount color-amount)
             (setq result nil)))
          ((string= color "blue")
           (when (< blue-cubes-amount color-amount)
             (setq result nil)))))
  result)

(defun get-game-id-if-possible
    (game-content red-cubes-amount green-cubes-amount blue-cubes-amount)
  "Returns possible game id, if not returns 0."
  (setq game-pair (split-string game-content ": "))
  (setq game-id (cdr (split-string (car game-pair) " ")))
  (setq game-hands (split-string (car (cdr game-pair)) "; "))

  ;; Check if any game-hand violates
  (setq possible-game-id (string-to-number (car game-id)))
  (dolist (game-hand game-hands)
    (unless (is-game-hand-possible game-hand red-cubes-amount green-cubes-amount blue-cubes-amount)
      (setq possible-game-id 0)))
  possible-game-id)

(defun get-game-id-if-possible-with-parameters (game)
  (get-game-id-if-possible game 12 13 14))

(defvar games puzzle-lines)

(reduce '+ (mapcar #'get-game-id-if-possible-with-parameters games))
