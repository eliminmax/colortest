\ SPDX-FileCopyrightText: 2023 - 2025 Eli Array Minkoff
\ SPDX-License-Identifier: GPL-3.0-only

( print a line-feed character )
: LF 10 EMIT ;

( clear terminal color formatting )
: CLEAR_FMT 27 EMIT ." [0m" ;

( print the color cell for the value at the top of the stack)
( consumes top value of stack )
: COLOR_CELL
    ( print ASCII escape )
    27 EMIT
    ( print start of sequence )
    ." [48;5;"
    ( print number from top of stack, without space )
    0 <# #S #> TYPE
    ( print end of sequence )
    ." m  "
;

( print a 6-cell long part of a cube row )
( restores stack to starting state )
: CUBE_ROW_PART 6 0 DO DUP I + COLOR_CELL LOOP CLEAR_FMT ;

( prints a full cube row startin with the color at top of stack )
( consumes top value of stack )
: CUBE_ROW
    CUBE_ROW_PART ."   "
    36 + CUBE_ROW_PART ."   "
    36 + CUBE_ROW_PART LF
    DROP
;

( Print the first 16 colors - these vary by terminal configuration )
( restores stack to starting state )
: PART_1 LF 16 0 DO I COLOR_CELL LOOP CLEAR_FMT LF LF ; PART_1

( Print the 6 sides of the color cube - these are more standardized,
  but the order is a bit odd, thus the need for this trickery )

( helper tool to prints the 3 parts of a row for part 2 )
( restores stack to starting state )
: PART_2
    6 0 DO I 6 * 16 + CUBE_ROW LOOP LF
    24 18 DO I 6 * 16 + CUBE_ROW LOOP LF
; PART_2

( Finally, the 24 grays )
( restores stack to starting state )
: PART_3 256 232 DO I COLOR_CELL LOOP CLEAR_FMT LF LF ; PART_3

