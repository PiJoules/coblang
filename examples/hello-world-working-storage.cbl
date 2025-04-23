       IDENTIFICATION DIVISION.
         PROGRAM-ID. WORKING-STORAGE.
       DATA DIVISION.
         WORKING-STORAGE SECTION.
           01 hello-world-str PIC X(15) VALUE "Hello world".
       PROCEDURE DIVISION.
         DISPLAY hello-world-str.
         EXIT PROGRAM.
