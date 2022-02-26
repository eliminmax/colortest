#!/bin/bf

### SET UP INITIAL VALUES

set cell 0 to ANSI escape code (CHAR 0x1b)
   4    8    c   10   14   18   1c
++++ ++++ ++++ ++++ ++++ ++++ +++

tape state is now 1b ……
tape pointer @     ^

> move to cell 1

set cell 1 to space (CHAR 0x20)
   4    8    c   10   14   18   1c   20
++++ ++++ ++++ ++++ ++++ ++++ ++++ ++++

tape state is now 1b 20 ……
tape pointer @        ^

> move to cell 2

set cell 2 to newline (CHAR 0x0a)
   4    8    c   10   14   18   1c   20
++++ ++++ ++

tape state is now 1b 20 0a ……
tape pointer @           ^

> move to cell 3

set cell 3 to open square bracket (can't type it because bf) (CHAR 0x5b)
   4    8    c   10   14   18   1c   20   24   28   2c   30   34   38   3c
++++ ++++ ++++ ++++ ++++ ++++ ++++ ++++ ++++ ++++ ++++ ++++ ++++ ++++ ++++ 
   40   44   48  4c   50   54   58   5c
++++ ++++ ++++ ++++ ++++ ++++ ++++ +++ 

tape state is now 1b 20 0a 5b ……
tape pointer @              ^

> move to cell 4

set cell 4 to ; (CHAR 0x3b)
   4    8    c   10   14   18   1c   20   24   28   2c   30   34   38   3c
++++ ++++ ++++ ++++ ++++ ++++ ++++ ++++ ++++ ++++ ++++ ++++ ++++ ++++ +++ 

tape state is now 1b 20 0a 5b 3b ……
tape pointer @                 ^

> move to cell 5

set cell 5 to '4' (CHAR 0c34) — this will be the first numeric printable
   4    8    c   10   14   18   1c   20   24   28   2c   30   34
++++ ++++ ++++ ++++ ++++ ++++ ++++ ++++ ++++ ++++ ++++ ++++ ++++

tape state is now 1b 20 0a 5b 3b 34 ……
tape pointer @                    ^

> move to cell 6

set cell 6 to 0 (the character) (CHAR 0x30)
   4    8    c   10   14   18   1c   20   24   28   2c   30
++++ ++++ ++++ ++++ ++++ ++++ ++++ ++++ ++++ ++++ ++++ ++++

tape state is now 1b 20 0a 5b 3b 34 30 ……
tape pointer @                       ^

> move to cell 7

set cell 7 to m (CHAR 0x6d)
   4    8    c   10   14   18   1c   20   24   28   2c   30   34   38   3c
++++ ++++ ++++ ++++ ++++ ++++ ++++ ++++ ++++ ++++ ++++ ++++ ++++ ++++ ++++ 
   40   44   48  4c   50   54   58   5c   60   64   68   6c   70
++++ ++++ ++++ ++++ ++++ ++++ ++++ ++++ ++++ ++++ ++++ ++++ +


tape state is now 1b 20 0a 5b 3b 34 30 6d …… ……
tape pointer @                          ^

### BEGIN OUTPUT

<<<< < return to cell 2 (newline)
. output newline



### First 16 colors 

go to cell 8 and set it to 0x0a

>>>> >>
   4    8    c
++++ ++++ ++

[ loop until a cell with value 0 is hit

<<<< <<<< return to cell 0 (ANSI escape code)
. begin escape code

>>> return to cell 3 (open square brackets)
. open escape code

>> return to cell 5 (numeric starting at 4)
.++++. output 4¸ add 4 to value¸ output 8

<.> print ; from previous cell and return to cell 5

---. decrement cell 5 down to 5 (the character) and print

- reset cell 5 to 4 (the character)

<.> print ; from previous cell and return to cell 5

>.+ go to cell 6 and print it out and increment it

>. go to cell 7 and print it out

<<<<<<..>>>>>> go to cell 1 and print it twice

>- go to cell 8 and decrement it by 1
]
