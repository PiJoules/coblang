       IDENTIFICATION DIVISION.
         PROGRAM-ID. cobl-strlen.
       DATA DIVISION.
         local-storage section.
           01 src-char-buffer pic x based.
           01 local-src-ptr usage pointer.

         LINKAGE SECTION.
           01 src-ptr usage pointer.
           01 size-return-arg usage binary-c-long unsigned.

      *
      * Effectively similar to strlen.
      *
       PROCEDURE DIVISION using src-ptr size-return-arg.
         move src-ptr to local-src-ptr.
         move 0 to size-return-arg.
         perform forever
           set address of src-char-buffer to local-src-ptr
           if src-char-buffer = x"00"
             exit perform
           end-if
           set size-return-arg up by 1
           set local-src-ptr up by 1
         end-perform.
         goback.
