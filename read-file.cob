       IDENTIFICATION DIVISION.
       PROGRAM-ID. READ-FILE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * EXPORT DD_INFILE=input-file in sh before running read_file.exe
           SELECT INFILE ASSIGN TO "DD_INFILE"
           ORGANIZATION IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  INFILE.
       01  RECORDIN.
           02  FILLER                             PIC X(64).
       WORKING-STORAGE SECTION.
       77  WS-NAME                                PIC X(64).
       77  WS-LINE-COUNTER                        PIC 99.
       PROCEDURE DIVISION.
       0000-MAIN.
           OPEN INPUT INFILE.
           MOVE 0 TO WS-LINE-COUNTER.
       0010-READ-FILE.
           READ INFILE AT END GO 0020-READ-FILE.
           MOVE RECORDIN TO WS-NAME.
           DISPLAY WS-LINE-COUNTER " " WS-NAME "<".
           ADD 1 TO WS-LINE-COUNTER.
           GO TO 0010-READ-FILE.
       0020-READ-FILE.
           CLOSE INFILE.
           DISPLAY WS-LINE-COUNTER " LINES READ.".
       9999-EXIT.
           STOP RUN.
