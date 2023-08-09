( helper tool to print single-length unsigned integer without trailing space )
: U.N 0 <# #S #> TYPE ;
( print the color cell )
: COLOR_CELL ESC[ ." 48;5;" U.N ." m  " ;

( Print the first 16 colors - these vary by terminal configuration )
: PART_1 CR 16 0 DO I COLOR_CELL LOOP ESC[ ." 0m" CR CR ; PART_1

( Print the 6 sides of the color cube - these are more standardized,
  but the order is a bit odd, thus the need for this trickery )

( helper tool to prints the 3 parts of a row for part 2 )
: ROW_A 6 0 DO DUP I + COLOR_CELL LOOP ESC[ ." 0m  " ;
: ROW_B 42 36 DO DUP I + COLOR_CELL LOOP ESC[ ." 0m  " ;
: ROW_C 78 72 DO DUP I + COLOR_CELL LOOP ESC[ ." 0m" CR ;

: PART_2A 6 0 DO I 6 * 16 + ROW_A ROW_B ROW_C LOOP CR ; PART_2A
: PART_2B 24 18 DO I 6 * 16 + ROW_A ROW_B ROW_C LOOP CR ; PART_2B

\ Finally, the 24 grays
: PART_3 256 232 DO I COLOR_CELL LOOP ESC[ ." 0m" CR CR ; PART_3
