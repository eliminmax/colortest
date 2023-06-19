#!/usr/bin/env -S clisp

; these variables come in handy
(defvar esc (list (code-char 27)))
(defvar nl (list (code-char 10)))
(defvar clearfmt (concatenate 'string esc "[0m"))
(defun stringofcolor (n)
    (concatenate 'string esc "[48;5;" (write-to-string n) "m  ")
)

; Print the first 16 colors - these vary by terminal configuration
(write-line "")
(let ((i 0))
    (loop 
        (write-string (stringofcolor i))
        (setq i (+ i 1))
        (when (< 15 i) (return))
    )
)
(write-string (concatenate 'string clearfmt nl nl))

; Print the 6 sides of the color cube - these are more standardized
; but the order is a bit odd, thus the need for this trickery
(let ((i 16))
    (loop
        (let ((ii 0))
            (loop
                (write-string (stringofcolor (+ i ii)))
                (setq ii (+ ii 1))
                (when (< 5 ii) (return))
            )
        )
        (write-string (concatenate 'string clearfmt "  "))
        (let ((ii 36))
            (loop
                (write-string (stringofcolor (+ i ii)))
                (setq ii (+ ii 1))
                (when (< 41 ii) (return))
            )
        )
        (write-string (concatenate 'string clearfmt "  "))
        (let ((ii 72))
            (loop
                (write-string (stringofcolor (+ i ii)))
                (setq ii (+ ii 1))
                (when (< 77 ii) (return))
            )
        )
        (write-string (concatenate 'string clearfmt nl))
        (setq i (+ i 6))
        (when (< 46 i) (return))
    )
)
(write-line "")
(let ((i 124))
    (loop
        (let ((ii 0))
            (loop
                (write-string (stringofcolor (+ i ii)))
                (setq ii (+ ii 1))
                (when (< 5 ii) (return))
            )
        )
        (write-string (concatenate 'string clearfmt "  "))
        (let ((ii 36))
            (loop
                (write-string (stringofcolor (+ i ii)))
                (setq ii (+ ii 1))
                (when (< 41 ii) (return))
            )
        )
        (write-string (concatenate 'string clearfmt "  "))
        (let ((ii 72))
            (loop
                (write-string (stringofcolor (+ i ii)))
                (setq ii (+ ii 1))
                (when (< 77 ii) (return))
            )
        )
        (write-string (concatenate 'string clearfmt nl))
        (setq i (+ i 6))
        (when (< 154 i) (return))
    )
)
(write-line "")

; Finally, the 24 grays
(let ((i 232))
    (loop 
        (write-string (stringofcolor i))
        (setq i (+ i 1))
        (when (< 255 i) (return))
    )
)
(write-string (concatenate 'string clearfmt nl nl))
