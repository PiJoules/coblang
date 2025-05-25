       IDENTIFICATION DIVISION.
         PROGRAM-ID. cobl-memcpy-example.
       DATA DIVISION.
         local-storage section.
           01 src-char-buffer pic x based.
           01 dst-char-buffer pic x based.
           01 local-src-ptr usage pointer.
           01 local-dst-ptr usage pointer.
           01 end-src-ptr usage pointer.

         LINKAGE SECTION.
           01 src-ptr usage pointer.
           01 dst-ptr usage pointer.
           01 cpy-size usage binary-c-long unsigned.

       PROCEDURE DIVISION using dst-ptr src-ptr cpy-size.
         move src-ptr to local-src-ptr.
         move dst-ptr to local-dst-ptr.
         move src-ptr to end-src-ptr.
         set end-src-ptr up by cpy-size.
         perform until local-src-ptr = end-src-ptr
           set address of src-char-buffer to local-src-ptr
           set address of dst-char-buffer to local-dst-ptr
           move src-char-buffer to dst-char-buffer
           set local-src-ptr up by 1 
           set local-dst-ptr up by 1 
         end-perform.
         goback.
