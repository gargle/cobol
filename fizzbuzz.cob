       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIZZBUZZ.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CONSOLE IS TERM.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  COUNTER                                PIC 9(04).
       77  QUOTIENT                               PIC 9(03).
       77  REST3                                  PIC 9(03).
       77  REST5                                  PIC 9(03).
       01  FIZZBUZZ-OUT                           PIC X(08).
       01  COUNTER-OUT REDEFINES FIZZBUZZ-OUT     PIC Z(08).
       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM FIZZBUZZ VARYING COUNTER FROM 1 BY 1
                   UNTIL COUNTER > 1000.
       9999-EXIT.
           STOP RUN.
       FIZZBUZZ SECTION.
           DIVIDE COUNTER BY 3 GIVING QUOTIENT REMAINDER REST3.
           DIVIDE COUNTER BY 5 GIVING QUOTIENT REMAINDER REST5.
           MOVE COUNTER TO COUNTER-OUT.
           EVALUATE REST3 ALSO REST5
               WHEN 0 ALSO 0
                   MOVE "FIZZBUZZ" TO FIZZBUZZ-OUT
               WHEN 0 ALSO NOT 0
                   MOVE "FIZZ" TO FIZZBUZZ-OUT
               WHEN NOT 0 ALSO 0
                   MOVE "BUZZ" TO FIZZBUZZ-OUT
           END-EVALUATE.
           IF COUNTER < 1000
               DISPLAY TRIM(FIZZBUZZ-OUT) " " WITH NO ADVANCING
                   UPON TERM
           ELSE
               DISPLAY TRIM(FIZZBUZZ-OUT) UPON TERM.
           EXIT.
