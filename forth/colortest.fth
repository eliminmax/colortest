\ SPDX-FileCopyrightText: 2023-2024 Eli Array Minkoff
\ SPDX-License-Identifier: GPL-3.0-only

( print single-length unsigned integer without trailing space )
( consumes top value of stack )
: U.N 0 <# #S #> TYPE ;

( print the color cell for the value at the top of the stack)
( consumes top value of stack )
: COLOR_CELL ESC[ ." 48;5;" U.N ." m  " ;

( print a 6-cell long part of a cube row )
( restores stack to starting state )
: CUBE_ROW_PART 6 0 DO DUP I + COLOR_CELL LOOP ESC[ ." 0m" ;

( prints a full cube row startin with the color at top of stack )
( consumes top value of stack )
: CUBE_ROW
    CUBE_ROW_PART ."   "
    36 + CUBE_ROW_PART ."   "
    36 + CUBE_ROW_PART CR
    DROP
;

( Print the first 16 colors - these vary by terminal configuration )
( restores stack to starting state )
: PART_1 CR 16 0 DO I COLOR_CELL LOOP ESC[ ." 0m" CR CR ; PART_1

( Print the 6 sides of the color cube - these are more standardized,
  but the order is a bit odd, thus the need for this trickery )

( helper tool to prints the 3 parts of a row for part 2 )
( restores stack to starting state )
: PART_2
    6 0 DO I 6 * 16 + CUBE_ROW LOOP CR
    24 18 DO I 6 * 16 + CUBE_ROW LOOP CR
; PART_2

( Finally, the 24 grays )
( restores stack to starting state )
: PART_3 256 232 DO I COLOR_CELL LOOP ESC[ ." 0m" CR CR ; PART_3

