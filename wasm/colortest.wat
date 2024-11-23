;; SPDX-FileCopyrightText: 2024 Eli Array Minkoff
;;
;; SPDX-License-Identifier: GPL-3.0-only


(module
  (import "wasi_snapshot_preview1" "fd_write"
    ;; fd_write takes 4 i32 arguments:
    ;; - fd:        a file descriptor
    ;; - *iovs:     an iovs array
    ;; - iovs_len:  the number of elements in iovs
    ;; - *nwritten: a pointer to memory to set to the number of written bytes
    ;; it returns 0 on success and nonzero on failure
    (func $fd_write (param i32 i32 i32 i32) (result i32)))

  (memory $mem 1)
  (export "memory" (memory $mem))

  ;; string data needed throughout
  (data (i32.const 0) "\1b[0m  \1b[48;5;\00\00\00\00\00\00\n")

  ;; different snippets of text memory needed (index; length):
  ;; "\1b[0m": (0; 4)
  ;; "\1b[0m  ": (0; 6)
  ;; "m  ": (3; 3)
  ;; "  ": (4; 2)
  ;; "\1b[48;5;" (6; 7)
  ;; "\n" (19, 1)
  ;; use bytes 20-23 for fd_write's *nwritten
  ;; use bytes 24-31 for the iovs array

  (func $write ;; write size bytes 
    (param $index i32)
    (param $size i32)
    (i32.store (i32.const 24) (local.get $index))
    (i32.store (i32.const 28) (local.get $size))
    (call $fd_write
          (i32.const 1) ;; stdout file descriptor
          (i32.const 24) ;; iovs array
          (i32.const 1) ;; length of 1
          (i32.const 20) ;; going to ignore this
    )
    (drop) ;; don't care about return value of fwrite, so clear it from the stack
  )

  (func $color_cell
    (param $n i32)
    (local $ptr i32)
    (local $len i32)

    (; need to first set the bytes after the color cell start to an ASCII
      representation of $n. ;)
    (; Assume that $n fits within the range of unsigned 8-bit values ;)

    ;; set ptr to the address of the first digit to write
    (local.set $ptr (i32.const 13))
    ;; set local $len to the number of digits needed to represent $n
    (if (i32.ge_u (local.get $n) (i32.const 100))
      (then (local.set $len (i32.const 13)))
      (else
        (if (i32.ge_u (local.get $n) (i32.const 10))
          (then (local.set $len (i32.const 12)))
          (else (local.set $len (i32.const 11)))
        )
      )
    )

    (block $hundreds
      ;; break out if length is less than 3
      (if (i32.lt_u (local.get $len) (i32.const 13)) (then (br $hundreds)))
      ;; push n to stack before anything else
      (; if $n is >= 200 then set first character to '2' and leave 200 on the
        stack to subtract from $n, otherwise set the first character to '1' and
        leave 100 on the stack ;)
      (if (i32.ge_u (local.get $n) (i32.const 200))
        (then
          (i32.store8 (local.get $ptr) (i32.const 0x32))
          (local.set $n (i32.sub (local.get $n) (i32.const 200)))
        )
        (else
          (i32.store8 (local.get $ptr) (i32.const 0x31))
          (local.set $n (i32.sub (local.get $n) (i32.const 100)))
        )
      )
      (; the top two values on the stack are ($n/100) and $n, so this sets
        $n to $n % 100 ;)
      ;; move $ptr to the next character
      (local.set $ptr (i32.add (local.get $ptr) (i32.const 1)))
    )

    (block $tens
      ;; break out if length is less than 2
      (if (i32.lt_u (local.get $len) (i32.const 12)) (then (br $tens)))
      ;; push $ptr to the stack
      local.get $ptr
      ;; push n/10 + '0' to stack
      (i32.add (i32.div_u (local.get $n) (i32.const 10)) (i32.const 0x30))
      i32.store8
      ;; set $n to $n % 10
      (local.set $n (i32.rem_u (local.get $n) (i32.const 10)))
      ;; move $ptr to the next character
      (local.set $ptr (i32.add (local.get $ptr) (i32.const 1)))
    )

    (block $ones
      ;; add ASCII '0' to turn it into a digit, then store the result at $ptr
      (i32.store8 (local.get $ptr) (i32.add (i32.const 0x30) (local.get $n)))
      ;; move $ptr to the next character
      (local.set $ptr (i32.add (local.get $ptr) (i32.const 1)))
    )
    ;; Set the 3 characters after the number text to "m  "
    (memory.copy (local.get $ptr) (i32.const 3) (i32.const 3))
    (call $write  (i32.const 6) (local.get $len))
  )

  (func $cube_row_part
    (param $n i32)
    ;; set $tgt to $n + 6
    (local $tgt i32)
    (local.set $tgt (local.get $n))
    (local.set $tgt (i32.add (local.get $tgt) (i32.const 6)))
    (loop $part_squares
      (call $color_cell (local.get $n))
      (local.set $n (i32.add (local.get $n) (i32.const 1)))
      ;; keep going until $n == $tgt
      (if (i32.lt_u (local.get $n) (local.get $tgt)) (then (br $part_squares)))
    )
  )

  (func $cube_row
    (param $n i32)
    (call $cube_row_part (local.get $n))
    (call $write (i32.const 0) (i32.const 6))
    (local.set $n (i32.add (local.get $n) (i32.const 36))) 
    (call $cube_row_part (local.get $n))
    (call $write (i32.const 0) (i32.const 6))
    (local.set $n (i32.add (local.get $n) (i32.const 36))) 
    (call $cube_row_part (local.get $n))
    (call $write (i32.const 0) (i32.const 4))
    (call $write (i32.const 19) (i32.const 1))
  )

  (func $main
    (local $i i32)
    (call $write (i32.const 19) (i32.const 1))
    ;; Print the first 16 colors - these vary by terminal configuration
    (local.set $i (i32.const 0))
    (loop $first_16_loop
      (call $color_cell (local.get $i))
      (local.set $i (i32.add (local.get $i) (i32.const 1)))
      (if (i32.lt_u (local.get $i) (i32.const 16)) (then (br $first_16_loop)))
    )
    ;; write the clear formatting sequence, followed by 2 newlines
    (call $write (i32.const 0) (i32.const 4))
    (call $write (i32.const 19) (i32.const 1))
    (call $write (i32.const 19) (i32.const 1))

    (; Print the 6 sides of the color cube - these are more standardized,
      but the order is a bit odd, thus the need for the above trickery ;)
    (loop $first_cube_half
      (call $cube_row (local.get $i))
      (local.set $i (i32.add (local.get $i) (i32.const 6)))
      (if
        (i32.lt_u (local.get $i) (i32.const 52))
        (then (br $first_cube_half))
      )
    )
    (call $write (i32.const 19) (i32.const 1))
    (local.set $i (i32.const 124))
    (loop $second_cube_half
      (call $cube_row (local.get $i))
      (local.set $i (i32.add (local.get $i) (i32.const 6)))
      (if
        (i32.lt_u (local.get $i) (i32.const 160))
        (then (br $second_cube_half))
      )
    )
    (call $write (i32.const 19) (i32.const 1))

    ;; Finally, the 24 grays.
    (local.set $i (i32.const 232))
    (loop $grays_loop
      (call $color_cell (local.get $i))
      (local.set $i (i32.add (local.get $i) (i32.const 1)))
      (if (i32.lt_u (local.get $i) (i32.const 256)) (then (br $grays_loop)))
    )
    ;; write the clear formatting sequence, followed by 2 newlines
    (call $write (i32.const 0) (i32.const 4))
    (call $write (i32.const 19) (i32.const 1))
    (call $write (i32.const 19) (i32.const 1))
  )
  (start $main)
)

;; vi: sw=2 ts=2 sts=2 et cc=80
