       IDENTIFICATION DIVISION.
       PROGRAM-ID. READ-VARIABLE-FILE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * EXPORT DD_INFILE=input-file in sh before running read_file.exe
           SELECT INFILE ASSIGN TO "DD_INFILE"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS INFILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  INFILE
           RECORD IS VARYING IN SIZE
           FROM 0 TO 32768 DEPENDING ON INFILE-SIZE.
       01  INFILEIN.
           02  PIC X OCCURS 0 TO 32768 DEPENDING ON INFILE-SIZE.
       WORKING-STORAGE SECTION.
       01  INFILE-STATUS                          PIC XX.
           88  INFILE-OK                          VALUE "00".
           88  INFILE-EOF                         VALUE "10".
       01  INFILE-SIZE                            PIC 9(5).
       PROCEDURE DIVISION.
       0000-MAIN.
           OPEN INPUT INFILE.
       0010-READ-FILE.
           READ INFILE AT END GO 0020-READ-FILE.
           IF INFILE-OK THEN
               DISPLAY INFILE-SIZE " , " INFILE-STATUS " : " INFILEIN
           ELSE
               DISPLAY "SOME ERROR OCCURED " INFILE-STATUS
           END-IF.
           GO TO 0010-READ-FILE.
       0020-READ-FILE.
           DISPLAY "EOF " INFILE-STATUS.
           CLOSE INFILE.
       9999-EXIT.
           STOP RUN.
