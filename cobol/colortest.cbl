       IDENTIFICATION DIVISION.
       PROGRAM-ID. colortest.
      * SPDX-FileCopyrightText: 2023-2024 Eli Array Minkoff
      * SPDX-License-Identifier: GPL-3.0-only
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SPECIAL-NAMES.
      * These are each one more than the ASCII values of the characters,
      * because a value of 1 corresponds with the NULL byte.
           SYMBOLIC CHARACTERS ESC IS 28.
           SYMBOLIC CHARACTERS LINE-FEED IS 11.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Number Display
           01 ND PICTURE ZZ9.
      * Primary iterator value
           01 I PICTURE 999 VALUE 0.
      * ColorCube-Row-Part iterator value
           01 ICRP PICTURE 999.
       PROCEDURE DIVISION.
       MAIN-PARA.
      *    Print the first 16 colors - these vary by terminal config
           DISPLAY LINE-FEED WITH NO ADVANCING
           PERFORM COLORCELL-PARA WITH TEST BEFORE UNTIL I=16
           DISPLAY ESC'[0m'LINE-FEED
      *    Print the 6 sides of the color cube - these are more
      *    standardized but the order is a bit odd, thus the need for
      *    the below trickery
           PERFORM COLORCUBE-ROW-PARA WITH TEST BEFORE UNTIL I=52
           DISPLAY LINE-FEED WITH NO ADVANCING
           MOVE 124 TO I
           PERFORM COLORCUBE-ROW-PARA WITH TEST BEFORE UNTIL I=160
           DISPLAY LINE-FEED WITH NO ADVANCING
      *    Finally, the 24 grays
           MOVE 232 TO I
           PERFORM COLORCELL-PARA WITH TEST BEFORE UNTIL I=256
           DISPLAY ESC"[0m"LINE-FEED
           STOP RUN.

       COLORCELL-PARA.
           MOVE I TO ND
           DISPLAY ESC"[48;5;"FUNCTION TRIM(ND)"m  " WITH NO ADVANCING
           ADD 1 TO I.

       COLORCUBE-ROW-PARA.
           MOVE 0 TO ICRP
           PERFORM CUBE-ROW-PART-PARA WITH TEST BEFORE UNTIL ICRP=6
           DISPLAY ESC"[0m  " WITH NO ADVANCING
           MOVE 36 TO ICRP
           PERFORM CUBE-ROW-PART-PARA WITH TEST BEFORE UNTIL ICRP=42
           DISPLAY ESC"[0m  " WITH NO ADVANCING
           MOVE 72 TO ICRP
           PERFORM CUBE-ROW-PART-PARA WITH TEST BEFORE UNTIL ICRP=78
           DISPLAY ESC"[0m"
           ADD 6 TO I.
      * Name abreviated to fit within above line limits
       CUBE-ROW-PART-PARA.
           ADD ICRP TO I
           MOVE I TO ND
           DISPLAY ESC"[48;5;"FUNCTION TRIM(ND)"m  " WITH NO ADVANCING
           SUBTRACT ICRP FROM I
           ADD 1 TO ICRP.
