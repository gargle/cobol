       IDENTIFICATION DIVISION.
       PROGRAM-ID. BIER.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CONSOLE IS TERM.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BIER ASSIGN TO "BIER.FILE"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FILE-STAT.
       DATA DIVISION.
       FILE SECTION.
       FD  BIER.
       01  BIER-RECORD.
           03  BIER-TIMESTAMP-NUMERIC             PIC 9(14).
           03  FILLER REDEFINES BIER-TIMESTAMP-NUMERIC.
               05  BIER-DATE-NUMERIC              PIC 9(08).
               05  BIER-DATE REDEFINES BIER-DATE-NUMERIC.
                   07  BIER-DATE-YEAR             PIC 9(04).
                   07  BIER-DATE-MONTH            PIC 9(02).
                   07  BIER-DATE-DAY              PIC 9(02).
               05  BIER-TIME-NUMERIC              PIC 9(06).
               05  BIER-TIME REDEFINES BIER-TIME-NUMERIC.
                   07  BIER-TIME-HOURS            PIC 9(02).
                   07  BIER-TIME-MINUTES          PIC 9(02).
                   07  BIER-TIME-SECONDS          PIC 9(02).
       WORKING-STORAGE SECTION.
       01  WS-CURRENT-DATE-DATA.
           03  WS-CURRENT-DATE                    PIC 9(08).
           03  FILLER REDEFINES WS-CURRENT-DATE.
               05  WS-CURRENT-YEAR                PIC 9(04).
               05  WS-CURRENT-MONTH               PIC 9(02).
               05  WS-CURRENT-DAY                 PIC 9(02).
           03  WS-CURRENT-TIME                    PIC 9(08).
           03  FILLER REDEFINES WS-CURRENT-TIME.
               05  WS-CURRENT-HOURS               PIC 9(02).
               05  WS-CURRENT-MINUTES             PIC 9(02).
               05  WS-CURRENT-SECONDS             PIC 9(02).
               05  WS-CURRENT-MILLISECONDS        PIC 9(02).
       01  FILLER REDEFINES WS-CURRENT-DATE-DATA.
           03  WS-CURRENT-TIMESTAMP-NUMERIC       PIC 9(14).
           03  FILLER                             PIC 9(02).
       77  WS-CURRENT-TIMESTAMP-DIFF              PIC 9(14).
       77  WS-BIER-COUNTER                        PIC 9(02).
       77  FILE-STAT                              PIC X(02).
       PROCEDURE DIVISION.
       0000-MAIN.
           MOVE CURRENT-DATE TO WS-CURRENT-DATE-DATA.
       0010-OPEN-FILE.
           OPEN INPUT BIER.
       0010-CHECK-FILE.
           IF FILE-STAT = "35" THEN
               GO TO 0200-NEW-FILE.
           MOVE 0 TO WS-BIER-COUNTER.
       0020-READ-FILE.
           READ BIER
               AT END GO TO 0090-CLOSE-FILE.
           SUBTRACT BIER-TIMESTAMP-NUMERIC
               FROM WS-CURRENT-TIMESTAMP-NUMERIC
               GIVING WS-CURRENT-TIMESTAMP-DIFF.
           ADD 1 TO WS-BIER-COUNTER.
           GO TO 0020-READ-FILE.
       0090-CLOSE-FILE.
           CLOSE BIER.
       0100-PROCESS-DIFFERENCE.
           IF WS-CURRENT-TIMESTAMP-DIFF >= 40000
               GO TO 0200-NEW-FILE.
       0200-OLD-FILE.
           OPEN EXTEND BIER.
           GO TO 0200-PROCESS-ONE-BIER.
       0200-NEW-FILE.
           OPEN OUTPUT BIER.
       0200-PROCESS-ONE-BIER.
           ADD 1 TO WS-BIER-COUNTER.
           MOVE WS-CURRENT-TIMESTAMP-NUMERIC TO BIER-TIMESTAMP-NUMERIC.
           WRITE BIER-RECORD.
       0200-CLOSE-FILE.
           CLOSE BIER.
       0300-DISPLAY-STATISTICS.
           DISPLAY "YOU HAD " WS-BIER-COUNTER " BEERS.  "
                   "YOUR LAST ONE WAS AT " WS-CURRENT-HOURS ":"
                                           WS-CURRENT-MINUTES ":"
                                           WS-CURRENT-SECONDS "."
                   UPON TERM.
       9999-EXIT.
           STOP-RUN.
