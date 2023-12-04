(with-temp-buffer
  (insert-file-contents "puzzle4_input.txt")
  (setq puzzle-lines (split-string (buffer-string) "\n" t)))

(defun required-cubes-for-game (game-hand)
  (setq hand-colors (split-string game-hand ", "))
  (setq red-amount 0)
  (setq green-amount 0)
  (setq blue-amount 0)

  (dolist (hand-color hand-colors)
    (setq color-info (split-string hand-color))
    (setq color-amount (string-to-number (car color-info)))
    (setq color (car (cdr color-info)))
    (cond ((string= color "red")
           (when (< red-amount color-amount)
             (setq red-amount color-amount)))
          ((string= color "green")
           (when (< green-amount color-amount)
             (setq green-amount color-amount)))
          ((string= color "blue")
           (when (< blue-amount color-amount)
             (setq blue-amount color-amount)))))
  (list red-amount green-amount blue-amount))

(defun get-required-cube-amounts-for-game (game-content)
  "Returns required cubes product, if not returns 0."
  (setq game-pair (split-string game-content ": "))
  (setq game-id (cdr (split-string (car game-pair) " ")))
  (setq game-hands (split-string (car (cdr game-pair)) "; "))

  ;; Check if any game-hand violates
  (setq required-red-amount 0)
  (setq required-green-amount 0)
  (setq required-blue-amount 0)

  (dolist (game-hand game-hands)
    (setq required-cubes (required-cubes-for-game game-hand))
    (setq required-red-amount (max required-red-amount (nth 0 required-cubes)))
    (setq required-green-amount (max required-green-amount (nth 1 required-cubes)))
    (setq required-blue-amount (max required-blue-amount (nth 2 required-cubes))))

  (list required-red-amount required-green-amount required-blue-amount))

(defun get-required-cube-amount-product-for-game (game)
  (reduce '* (get-required-cube-amounts-for-game game)))

(defvar games puzzle-lines)

(reduce '+ (mapcar #'get-required-cube-amount-product-for-game games))
