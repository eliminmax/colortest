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
           01 I PICTURE 999.
      * ColorCube-Row-Part iterator value
           01 ICRP PICTURE 999.
       PROCEDURE DIVISION.
       MAIN-PARA.
      *    Print the first 16 colors - these vary by terminal config
           DISPLAY LINE-FEED WITH NO ADVANCING
           PERFORM COLORCELL-PARA TEST BEFORE VARYING I
           FROM 0 BY 1 UNTIL I=16
           DISPLAY ESC'[0m'LINE-FEED
      *    Print the 6 sides of the color cube - these are more
      *    standardized but the order is a bit odd, thus the need for
      *    the below trickery
           PERFORM COLORCUBE-ROW-PARA TEST BEFORE VARYING I
           FROM 16 BY 6 UNTIL I=52
           DISPLAY LINE-FEED WITH NO ADVANCING
           PERFORM COLORCUBE-ROW-PARA TEST BEFORE VARYING I
           FROM 124 BY 6 UNTIL I=160
           DISPLAY LINE-FEED WITH NO ADVANCING
      *    Finally, the 24 grays
           PERFORM COLORCELL-PARA TEST BEFORE VARYING I
           FROM 232 BY 1 UNTIL I=256
           DISPLAY ESC"[0m"LINE-FEED
           STOP RUN.

       COLORCELL-PARA.
           MOVE I TO ND
           DISPLAY ESC"[48;5;"FUNCTION TRIM(ND)"m  " WITH NO ADVANCING.

       COLORCUBE-ROW-PARA.
           PERFORM CUBE-ROW-PART-PARA TEST BEFORE VARYING ICRP
           FROM 0 BY 1 UNTIL ICRP=6
           DISPLAY ESC"[0m  " WITH NO ADVANCING
           PERFORM CUBE-ROW-PART-PARA TEST BEFORE VARYING ICRP
           FROM 36 by 1 UNTIL ICRP=42
           DISPLAY ESC"[0m  " WITH NO ADVANCING
           PERFORM CUBE-ROW-PART-PARA TEST BEFORE VARYING ICRP
           FROM 72 BY 1 UNTIL ICRP=78
           DISPLAY ESC"[0m".

       CUBE-ROW-PART-PARA.
           ADD ICRP TO I
           MOVE I TO ND
           PERFORM COLORCELL-PARA.
           SUBTRACT ICRP FROM I.
