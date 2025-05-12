       IDENTIFICATION DIVISION.
         PROGRAM-ID. ENTRY.
       DATA DIVISION.
         WORKING-STORAGE SECTION.
           01 str PIC X(15).
         LINKAGE SECTION.
           01 str-arg PIC X(15).
       PROCEDURE DIVISION.
         MOVE "Hello" TO str.
         PERFORM PARA.
         GOBACK.
       
       ENTRY "ENTRY2" USING str-arg.
         MOVE str-arg TO str.
         PERFORM PARA.
         GOBACK.

       PARA.
         DISPLAY str.
       END-PARA.
