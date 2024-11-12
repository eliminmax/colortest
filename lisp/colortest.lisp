#!/usr/bin/env -S clisp

; SPDX-FileCopyrightText: 2023-2024 Eli Array Minkoff
;
; SPDX-License-Identifier: GPL-3.0-only

; these variables come in handy
(defvar esc (list (code-char 27)))
(defvar newline (list (code-char 10)))
(defvar clearfmt (concatenate 'string esc "[0m"))
(defun color_cell (n)
  (write-string (concatenate 'string esc "[48;5;" (write-to-string n) "m  ")))
(defun cube_row_part (n)
  (let ((i n))
    (loop
      (color_cell i)
      (setq i (+ i 1))
      (when (< (+ n 5) i) (return))))
  (write-string clearfmt))

(defun cube_row (n)
  (cube_row_part n)
  (write-string "  ")
  (cube_row_part (+ n 36))
  (write-string "  ")
  (cube_row_part (+ n 72))
  (write-line ""))

; Print the first 16 colors - these vary by terminal configuration
(write-line "")
(let ((i 0))
  (loop
    (color_cell i)
    (setq i (+ i 1))
    (when (< 15 i) (return))))
(write-string (concatenate 'string clearfmt newline newline))

; Print the 6 sides of the color cube - these are more standardized
; but the order is a bit odd, thus the need for this trickery
(let ((i 16))
  (loop
    (cube_row i)
    (setq i (+ i 6))
    (when (< 46 i) (return))))
(write-line "")
(let ((i 124))
  (loop
    (cube_row i)
    (setq i (+ i 6))
    (when (< 154 i) (return))))
(write-line "")

; Finally, the 24 grays
(let ((i 232))
  (loop
    (color_cell i)
    (setq i (+ i 1))
    (when (< 255 i) (return))))
(write-string (concatenate 'string clearfmt newline newline))
