       IDENTIFICATION DIVISION.
         PROGRAM-ID. CALL-ENTRY.
       DATA DIVISION.
         WORKING-STORAGE SECTION.
           01 arg PIC X(15).
       PROCEDURE DIVISION.
         CALL "ENTRY".
         MOVE "world!" TO arg.
         CALL "ENTRY2" USING arg.
         EXIT PROGRAM.
