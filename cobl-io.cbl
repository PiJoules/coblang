       IDENTIFICATION DIVISION.
         PROGRAM-ID. cobl-io.
       environment division.
         input-output section.
           file-control.
             select io-file
             assign to io-filename
             organization is sequential
             file status is file-status.
       DATA DIVISION.
         file section.
           fd io-file.
           01 io-file-char pic x.

         working-storage section.

         local-storage section.
           01 io-filename PIC X(128).
           01 file-status.
              05 fs1 pic x.
              05 fs2 pic x.

         LINKAGE SECTION.

       PROCEDURE DIVISION.
         stop run.

       entry "io-fopen".
         open input io-file.
         goback.
