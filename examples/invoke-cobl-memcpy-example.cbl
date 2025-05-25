       IDENTIFICATION DIVISION.
         PROGRAM-ID. invoke-cobl-memcpy-example.
       DATA DIVISION.
         local-storage section.
           01 src pic x(10).
           01 dst pic x(10).

       procedure division.
         move "abc123" to src.
         call "cobl-memcpy-example" using
              address of dst
              address of src
              10.
         display dst.
