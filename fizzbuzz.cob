       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIZZBUZZ.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CONSOLE IS TERM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  COUNTER                                PIC 9(04).
       77  QUOTIENT                               PIC 9(03).
       77  REST3                                  PIC 9(03).
       77  REST5                                  PIC 9(03).
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM FIZZBUZZ VARYING COUNTER FROM 1 BY 1
                   UNTIL COUNTER > 1000.
       9999-EXIT.
           STOP RUN.
       FIZZBUZZ SECTION.
           DIVIDE COUNTER BY 3 GIVING QUOTIENT REMAINDER REST3.
           DIVIDE COUNTER BY 5 GIVING QUOTIENT REMAINDER REST5.
           EVALUATE REST3 ALSO REST5
               WHEN 0 ALSO 0
                   DISPLAY "FIZZBUZZ" WITH NO ADVANCING UPON TERM
               WHEN 0 ALSO NOT 0
                   DISPLAY "FIZZ" WITH NO ADVANCING UPON TERM
               WHEN NOT 0 ALSO 0
                   DISPLAY "BUZZ" WITH NO ADVANCING UPON TERM
               WHEN NOT 0 ALSO NOT 0
                   DISPLAY COUNTER WITH NO ADVANCING UPON TERM
           END-EVALUATE.
           IF COUNTER < 1000
               DISPLAY " " WITH NO ADVANCING UPON TERM.
           EXIT.
