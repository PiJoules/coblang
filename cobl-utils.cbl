       IDENTIFICATION DIVISION.
         PROGRAM-ID. utils.
       DATA DIVISION.
         working-storage section.
           01 src-char-buffer pic x based.
           01 dst-char-buffer pic x based.

         local-storage section.
           01 src-ptr usage pointer.
           01 dst-ptr usage pointer.

           01 newcap usage binary-c-long unsigned.

         LINKAGE SECTION.
           01 src-ptr-arg usage pointer.

       PROCEDURE DIVISION.
         stop run.

       entry "print-c-string" using src-ptr-arg.
         move src-ptr-arg to src-ptr.
         perform forever
           set address of src-char-buffer to src-ptr
           if src-char-buffer = x"00" 
             exit perform
           end-if

           display src-char-buffer with no advancing
           set src-ptr up by 1
         end-perform.
         goback.
