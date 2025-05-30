       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
      * cobc -x -Wall hello-world.cob -T hello-world.lst -Xref
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WS-NAME                                PIC X(64).            here we store our name
       PROCEDURE DIVISION.
       0000-MAIN.
           DISPLAY "WHAT'S YOUR NAME : " WITH NO ADVANCING.
           ACCEPT WS-NAME.
           IF WS-NAME = SPACES THEN
               GO TO 9999-ABNORMAL-END.
           DISPLAY "HELLO " TRIM(WS-NAME) "!".
       9999-EXIT.
           STOP RUN WITH ERROR STATUS 0.
       9999-ABNORMAL-END.
           STOP RUN WITH ERROR STATUS 255.
