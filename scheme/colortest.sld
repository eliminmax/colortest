; SPDX-FileCopyrightText: 2024 Eli Array Minkoff
;
; SPDX-License-Identifier: GPL-3.0-only

(define (color_cell n)
  (display "\x1b[48;5;") (display (number->string n)) (display "m  ")
)

(define (range lo hi step)
  ; Return an empty list if step <= 0 or lo >= hi
  (cond ((<= step 0) '())
        ((>= lo hi) '())
      ; return lo combined with the next iteration's output
        (else (cons lo (range (+ lo step) hi step)))))

(define (cube_row_part n) (map color_cell (range n (+ n 6) 1)))
(define (cube_row n)
  (cube_row_part n)
  (display "\x1b[0m  ")
  (cube_row_part (+ n 36))
  (display "\x1b[0m  ")
  (cube_row_part (+ n 72))
  (display "\x1b[0m\n"))

; Print the first 16 colors - these vary by terminal configuration
(newline)
(map color_cell (range 0 16 1))
(display "\x1b[0m\n\n")

; Print the 6 sides of the color cube - these are more standardized,
; but the order is a bit odd, thus the need for the above trickery
(map cube_row (range 16 52 6))
(newline)
(map cube_row (range 124 160 6))
(newline)

; Finally, the 24 grays
(map color_cell (range 232 256 1))
(display "\x1b[0m\n\n")
