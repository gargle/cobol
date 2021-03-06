       IDENTIFICATION DIVISION.
       PROGRAM-ID. READ-VARIABLE-FILE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * EXPORT DD_OUTFILE=output-file in sh before running write_file.exe
           SELECT OUTFILE ASSIGN TO "DD_OUTFILE"
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS IS OUTFILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  OUTFILE
           RECORD IS VARYING IN SIZE
           FROM 0 TO 32768 DEPENDING ON OUTFILE-SIZE.
       01  OUTFILE-RECORD.
           02  PIC X OCCURS 0 TO 32768 DEPENDING ON OUTFILE-SIZE.
       WORKING-STORAGE SECTION.
       01  OUTFILE-STATUS                         PIC XX.
           88  OUTFILE-OK                         VALUE "00".
           88  OUTFILE-EOF                        VALUE "10".
       01  OUTFILE-SIZE                           PIC 9(5).
       PROCEDURE DIVISION.
       0000-MAIN.
           OPEN OUTPUT OUTFILE.

      * it is important to set the length first, and then fill in the record
           MOVE 10 TO OUTFILE-SIZE.
           MOVE "12345678901234" TO OUTFILE-RECORD.
           WRITE OUTFILE-RECORD.
           DISPLAY OUTFILE-STATUS.

           MOVE 14 TO OUTFILE-SIZE.
           MOVE "12345678901234" TO OUTFILE-RECORD.
           WRITE OUTFILE-RECORD.
           DISPLAY OUTFILE-STATUS.
           
           MOVE 3 TO OUTFILE-SIZE.
           MOVE "12345678901234" TO OUTFILE-RECORD.
           WRITE OUTFILE-RECORD.
           DISPLAY OUTFILE-STATUS.
           
           MOVE 6 TO OUTFILE-SIZE.
           MOVE "12345678901234" TO OUTFILE-RECORD.
           WRITE OUTFILE-RECORD.
           DISPLAY OUTFILE-STATUS.
           
           CLOSE OUTFILE.

       9999-EXIT.
           STOP RUN.
