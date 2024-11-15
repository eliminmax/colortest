       IDENTIFICATION DIVISION.
       PROGRAM-ID. colortest.
      * SPDX-FileCopyrightText: 2023-2024 Eli Array Minkoff
      * SPDX-License-Identifier: GPL-3.0-only
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Number Display
           01 ND PIC ZZ9 VALUE 0.
      * Primary iterator value
           01 I PIC 999 VALUE 0.
      * ColorCube-Row-Part iterator value
           01 ICRP PIC 999 VALUE 0.
       PROCEDURE DIVISION.
           MAIN-PARA.
      *    Print the first 16 colors - these vary by terminal configuration
           DISPLAY X'0a' WITH NO ADVANCING
           PERFORM COLORCELL-PARA WITH TEST BEFORE UNTIL I=16
           DISPLAY X'1b'"[0m"X'0a'
      *    Print the 6 sides of the color cube - these are more
      *    standardized but the order is a bit odd, thus the need for
      *    the below trickery
           PERFORM COLORCUBE-ROW-PARA WITH TEST BEFORE UNTIL I=52
           DISPLAY X'0a' WITH NO ADVANCING
           MOVE 124 TO I
           PERFORM COLORCUBE-ROW-PARA WITH TEST BEFORE UNTIL I=160
           DISPLAY X'0a' WITH NO ADVANCING
      *    Finally, the 24 grays
           MOVE 232 TO I
           PERFORM COLORCELL-PARA WITH TEST BEFORE UNTIL I=256
           DISPLAY X'1b'"[0m"X'0a'
           STOP RUN.

           COLORCELL-PARA.
           MOVE I TO ND
           DISPLAY X'1b'"[48;5;"FUNCTION TRIM(ND)"m  " WITH NO ADVANCING
           ADD 1 TO I.

           COLORCUBE-ROW-PARA.
           MOVE 0 TO ICRP
           PERFORM COLRCBE-ROW-PART-PARA WITH TEST BEFORE UNTIL ICRP=6
           DISPLAY X'1b'"[0m  " WITH NO ADVANCING
           MOVE 36 TO ICRP
           PERFORM COLRCBE-ROW-PART-PARA WITH TEST BEFORE UNTIL ICRP=42
           DISPLAY X'1b'"[0m  " WITH NO ADVANCING
           MOVE 72 TO ICRP
           PERFORM COLRCBE-ROW-PART-PARA WITH TEST BEFORE UNTIL ICRP=78
           DISPLAY X'1b'"[0m"
           ADD 6 TO I.
      * Name abreviated to fit within above line limits
           COLRCBE-ROW-PART-PARA.
           ADD ICRP TO I
           MOVE I TO ND
           DISPLAY X'1b'"[48;5;"FUNCTION TRIM(ND)"m  " WITH NO ADVANCING
           SUBTRACT ICRP FROM I
           ADD 1 TO ICRP.
