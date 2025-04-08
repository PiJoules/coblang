       IDENTIFICATION DIVISION.
         PROGRAM-ID. trimmed-string-length.
       DATA DIVISION.
         LINKAGE SECTION.
           01 RESULT  PIC 9(2).
           01 PARAM   PIC 9(2).
       PROCEDURE DIVISION USING PARAM, RESULT.
         DISPLAY 'Hello, world 2'.
         COMPUTE RESULT = PARAM + 1.
         EXIT PROGRAM.
