; SPDX-FileCopyrightText: 2024 Eli Array Minkoff
;
; SPDX-License-Identifier: GPL-3.0-only


(define (colorcell n)
  (display "\x1b[48;5;") (display (number->string n)) (display "m  ")
)

(define (range lo hi step)
  ; Return an empty list if step <= 0 or lo >= hi
  (cond ((<= step 0) '())
        ((>= lo hi) '())
      ; return lo combined with the next iteration's output
        (else (cons lo (range (+ lo step) hi step)))))

; Print the first 16 colors - these vary by terminal configuration
(newline)
(map colorcell (range 0 16 1))
(display "\x1b[0m\n\n")

; Print the 6 sides of the color cube - these are more standardized,
; but the order is a bit odd, thus the need for this trickery
(define (row_a n) (range n (+ n 6) 1))
(define (row_b n) (range (+ n 36) (+ n 42) 1))
(define (row_c n) (range (+ n 72) (+ n 78) 1))
(define (part_2_row n)
  (map colorcell (row_a n))
  (display "\x1b[0m  ")
  (map colorcell (row_b n))
  (display "\x1b[0m  ")
  (map colorcell (row_c n))
  (display "\x1b[0m\n"))

(map part_2_row (range 16 52 6))
(newline)
(map part_2_row (range 124 160 6))
(newline)

; Finally, the 24 grays
(map colorcell (range 232 256 1))
(display "\x1b[0m\n\n")
