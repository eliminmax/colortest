#!/usr/bin/env bf
# vi:sw=5:ts=5:sts=5:et:nowrap:noai
In these comments cells are zero indexed and cell values are hex unless specified otherwise

Not particularly optimized here


[
Code in this loop will never execute, making it effectively a comment
Used snippets from the Esolang wiki:
 - https://esolangs.org/wiki/Brainfuck_algorithms
 - https://esolangs.org/wiki/Brainfuck_constants
wherever used, I'll credit it with the comment ({algorithm|constant} {name} from esolang wiki)
If the algorithm name has characters with meaning in brainfuck, I will substitue or delete those characters
]


@@@@ STEP 0 @@@@


initialize cell to the sequence |00|00|00|0a|1b|5b|34|38|3b|35|20|6d|…


@ STEP 0:0 initialize first four cells to |00|00|00|10|

(constant 16 nonwrapping from esolang wiki)
>>++++[>++++<-]>

@ STEP 0:1 set each cell to the closest multiple of 16 that's less than its intended value
[
-         >    00
+         >    10
+++++     >    50
+++       >    30
+++       >    30
+++       >    30
+++       >    30
++        >    20
++++++    >    60
<<<<<<<<<      back to cell 2
]

@ STEP 0:2 finish setting values

++++++++++    > 0a
+++++++++++   > 1b
+++++++++++   > 5b
++++          > 34
++++++++      > 38
+++++++++++   > 3b
+++++         > 35
              > 20
+++++++++++++   6d
<<<<<<<<<<      back to cell 1


@@@@ STEP 1 @@@@

first 16 colors

@ STEP 1:0 create counter in cell 2

(constant 16 nonwrapping from esolang wiki)
++++[>++++<-]>

cells are now as follows
  0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f …
|00|00|10|0a|1b|5b|34|38|3b|35|20|6d|00|00|00|00|…

pointer is at cell 2

@ STEP 1:1 loop

print newline
>.<
[
     >>
     print escape sequence
     .>   ESC
     .>   Open square bracket
     .>   '4'
     .>   '8'
     .>   ';'
     .<   '5'
     .>   ';'
     >>> jump ahead to cell c

     Print decimal representation of cell c
     (algorithm "Print value of cell x as number (8 bit)" from esolang wiki)
     >>++++++++++<<[->+>-[>+>>]>[+[-<+>]>+>>]<<<<<<]>>[-]>>>++++++++++<[->-[>+>>]>[+[-
     <+>]>+>>]<<<<<]>[-]>>[>++++++[-<++++++++>]<.<<+>+>[-]]<[<[->-<]++++++[->++++++++
     <]>.[-]]<<++++++[-<++++++++>]<.[-]<<[-<+>]<
     That algorithm cleans itself up (putting us back at cell c with its initial value)

     add one for next round and finish printing escape sequence
     +<
     .<   'm' to close off escape sequence
     ..   ' ' * 2
     back to cell 2 (from cell a)
     <<<<<<<<
     - decrement counter
]


@ STEP 1:2 end escape and newlines to end section

currently at cell 2
>
>.             ESC
>.             Open square bracket
>----.++++     '0' using 34 cell (cell 6)
>>>>>.         'm'
currently at cell b


<<<<<<<< back to cell 3
newlines
..

@@@@ STEP 2 @@@@

color cube hell

@ STEP 2:0 set up first hell loop

each row has 3 segments of 6 colors each

going to be a bit of a verbose pseudocode to avoid characters with meaning in brainfuck
(all numbers in decimal here)
with a staring number i we want:
     i plus x for x in 0 through 6 followed by
     i plus x for x in 36 through 42 followed by
     i plus x for x in 72 through 78

  0  1  2  3  4  5  6  7  8  9  a  b  c …
|00|00|00|0a|1b|5b|34|38|3b|35|20|6d|10|…
           ^
at cell 3

<
++++++
<
++
<
++++++

  0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f …
|06|02|06|0a|1b|5b|34|38|3b|35|20|6d|10|00|00|00|…
  ^

@ STEP 2:1 first hell loop
[ outer loop starts at cell 0
     ->
     [    middle loop starts at cell 1
          -> countdown and start inner loop
          [ inner loop starts at cell 2
               go to cell 4
               >>
               print start of escape sequence
               .>   ESC
               .>   Open square bracket
               .>   '4'
               .>   '8'
               .>   ';'
               .<   '5'
               .>   ';'
               go to cell c
               >>>
               (algorithm "Print value of cell x as number (8 bit)" from esolang wiki)
               >>++++++++++<<[->+>-[>+>>]>[+[-<+>]>+>>]<<<<<<]>>[-]>>>++++++++++<[->-[>+>>]>[+[-
               <+>]>+>>]<<<<<]>[-]>>[>++++++[-<++++++++>]<.<<+>+>[-]]<[<[->-<]++++++[->++++++++
               <]>.[-]]<<++++++[-<++++++++>]<.[-]<<[-<+>]<
               + add one for next run
               <.<.. finish escape sequence and print spaces
               <<<<<<<< back to cell 2
               - countdown
          ]
          reset inner counter
          ++++++
          add 30 to cell c then go to cell 4
          >>>>>>>>>>++++++++++++++++++++++++++++++<<<<<<<<
          blank column then go to cell 1
          .>.>----.++++>>>>>.<..<<<<<<<<<
     ]
     final sextuple ends with newline rather than space so it's not in the middle loop above
 > to cell 2
     [ inner loop starts at cell 2
          go to cell 4
          >>
          print start of escape sequence
          .>   ESC
          .>   Open square bracket
          .>   '4'
          .>   '8'
          .>   ';'
          .<   '5'
          .>   ';'
          go to cell c
          >>>
          (algorithm "Print value of cell x as number (8 bit)" from esolang wiki)
          >>++++++++++<<[->+>-[>+>>]>[+[-<+>]>+>>]<<<<<<]>>[-]>>>++++++++++<[->-[>+>>]>[+[-
          <+>]>+>>]<<<<<]>[-]>>[>++++++[-<++++++++>]<.<<+>+>[-]]<[<[->-<]++++++[->++++++++
          <]>.[-]]<<++++++[-<++++++++>]<.[-]<<[-<+>]<
          + add one for next run
          <.<.. finish escape sequence and print spaces
          <<<<<<<< back to cell 2
          - countdown
     ]
     reset counter
     ++++++
     clear ANSI formatting
     >>.>.>----.++++>>>>>.
     subtract 0x48 from cell c
     >------------------------------------------------------------------------
     new line
     <<<<<<<<<.<<
     reset counter
     ++
     back to cell 0
     <
]

@ STEP 2:2 between the halves

newline
>>>.
>>>>>>>>>
add 0x48 to cell c
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Back to cell 0
<<<<<<<<<<<<
Set up counter
++++++

@ STEP 2:3 hell loop 2
[ outer loop starts at cell 0
     ->
     [    middle loop starts at cell 1
          -> countdown and start inner loop
          [ inner loop starts at cell 2
               go to cell 4
               >>
               print start of escape sequence
               .>   ESC
               .>   Open square bracket
               .>   '4'
               .>   '8'
               .>   ';'
               .<   '5'
               .>   ';'
               go to cell c
               >>>
               (algorithm "Print value of cell x as number (8 bit)" from esolang wiki)
               >>++++++++++<<[->+>-[>+>>]>[+[-<+>]>+>>]<<<<<<]>>[-]>>>++++++++++<[->-[>+>>]>[+[-
               <+>]>+>>]<<<<<]>[-]>>[>++++++[-<++++++++>]<.<<+>+>[-]]<[<[->-<]++++++[->++++++++
               <]>.[-]]<<++++++[-<++++++++>]<.[-]<<[-<+>]<
               + add one for next run
               <.<.. finish escape sequence and print spaces
               <<<<<<<< back to cell 2
               - countdown
          ]
          reset inner counter
          ++++++
          add 30 to cell c then go to cell 4
          >>>>>>>>>>++++++++++++++++++++++++++++++<<<<<<<<
          blank column then go to cell 1
          .>.>----.++++>>>>>.<..<<<<<<<<<
     ]
     final sextuple ends with newline rather than space so it's not in the middle loop above
 > to cell 2
     [ inner loop starts at cell 2
          go to cell 4
          >>
          print start of escape sequence
          .>   ESC
          .>   Open square bracket
          .>   '4'
          .>   '8'
          .>   ';'
          .<   '5'
          .>   ';'
          go to cell c
          >>>
          (algorithm "Print value of cell x as number (8 bit)" from esolang wiki)
          >>++++++++++<<[->+>-[>+>>]>[+[-<+>]>+>>]<<<<<<]>>[-]>>>++++++++++<[->-[>+>>]>[+[-
          <+>]>+>>]<<<<<]>[-]>>[>++++++[-<++++++++>]<.<<+>+>[-]]<[<[->-<]++++++[->++++++++
          <]>.[-]]<<++++++[-<++++++++>]<.[-]<<[-<+>]<
          + add one for next run
          <.<.. finish escape sequence and print spaces
          <<<<<<<< back to cell 2
          - countdown
     ]
     reset counter
     ++++++
     clear ANSI formatting
     >>.>.>----.++++>>>>>.
     subtract 0x48 from cell c
     >------------------------------------------------------------------------
     new line
     <<<<<<<<<.<<
     reset counter
     ++
     back to cell 0
     <
]

@ STEP 2:4 cleanup

>>>.
>>>>>>>>>
add 0x48 to cell c
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Back to cell 1
<<<<<<<<<<<

@@@@ STEP 3 @@@@

@ STEP 3:1

set up counter for final section

cell 1 starts at 2 and cell 2 starts at 6

do the following to get cell 2 to 24

[->+++++++++<]>

@ STEP 3:2 final loop

  0  1  2  3  4  5  6  7  8  9  a  b  c …
|00|00|18|0a|1b|5b|34|38|3b|35|20|6d|e8|…
        ^
[
     - countdown
     escape sequence start
     >>.  ESC
     >.   Open square bracket
     >.   '4'
     >.   '8'
     >.   ';'
     >.   '5'
     <.   ';'
     go to cell c
     >>>>
     (algorithm "Print value of cell x as number (8 bit)" from esolang wiki)
     >>++++++++++<<[->+>-[>+>>]>[+[-<+>]>+>>]<<<<<<]>>[-]>>>++++++++++<[->-[>+>>]>[+[-
     <+>]>+>>]<<<<<]>[-]>>[>++++++[-<++++++++>]<.<<+>+>[-]]<[<[->-<]++++++[->++++++++
     <]>.[-]]<<++++++[-<++++++++>]<.[-]<<[-<+>]<
     + add one for next run
     <.<.. finish escape sequence and print spaces
     <<<<<<<< back to cell 2
]
clear escape formatting
>>.>.>----.++++>>>>>.

back to cell 3 for the final newlines
<<<<<<<<..
