       IDENTIFICATION DIVISION.
       PROGRAM-ID. colortest.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 N PIC ZZ9 VALUE 0.
           01 I PIC 999 VALUE 0.
           01 II PIC 999 VALUE 0.
           01 TMP PIC 999 VALUE 0.
       PROCEDURE DIVISION.
           A-PARA.
      *    Print the first 16 colors - these vary by terminal configuration
           DISPLAY X'0a' WITH NO ADVANCING
           PERFORM B-PARA WITH TEST BEFORE UNTIL I=16
           DISPLAY X'1b'"[0m"X'0a'
      *    Print the 6 sides of the color cube - these are more
      *    standardized but the order is a bit odd, thus the need for
      *    this trickery
           PERFORM C-PARA WITH TEST BEFORE UNTIL I=52
           DISPLAY X'0a' WITH NO ADVANCING
           MOVE 124 TO I
           PERFORM C-PARA WITH TEST BEFORE UNTIL I=160
           DISPLAY X'0a' WITH NO ADVANCING
      *    Finally, the 24 grays
           MOVE 232 TO I
           PERFORM B-PARA WITH TEST BEFORE UNTIL I=256
           DISPLAY X'1b'"[0m"X'0a'
           STOP RUN.

           B-PARA.
           MOVE I TO N
           DISPLAY X'1b'"[48;5;"FUNCTION TRIM(N)"m  " WITH NO ADVANCING
           ADD 1 TO I.

           C-PARA.
           MOVE 0 TO II
           PERFORM D-PARA WITH TEST BEFORE UNTIL II=6
           DISPLAY X'1b'"[0m  " WITH NO ADVANCING
           MOVE 36 TO II
           PERFORM D-PARA WITH TEST BEFORE UNTIL II=42
           DISPLAY X'1b'"[0m  " WITH NO ADVANCING
           MOVE 72 TO II
           PERFORM D-PARA WITH TEST BEFORE UNTIL II=78
           DISPLAY X'1b'"[0m"
           ADD 6 TO I.

           D-PARA.
           MOVE I TO TMP
           ADD II TO TMP
           MOVE TMP TO N
           DISPLAY X'1b'"[48;5;"FUNCTION TRIM(N)"m  " WITH NO ADVANCING
           ADD 1 TO II.
