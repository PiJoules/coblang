       IDENTIFICATION DIVISION.
         PROGRAM-ID. cobl-getc.
       environment division.
         input-output section.
           file-control.
             select file-handle
             assign to disk
             organization is sequential.
       DATA DIVISION.
         file section.
           fd file-handle external.
             01 char pic X.

         LINKAGE SECTION.
           01 arr-size pic 99 value 10.
           01 arr-size2 pic 99 value 10.
           01 arr.
              05 elem pic x occurs 1 to 2
                 times depending on arr-size.

       PROCEDURE DIVISION USING arr-size.
         move arr-size to arr-size2.
         EXIT PROGRAM.
