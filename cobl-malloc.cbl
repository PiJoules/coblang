       IDENTIFICATION DIVISION.
         PROGRAM-ID. cobl-malloc.
       DATA DIVISION.
         LINKAGE SECTION.
           01 alloc-size usage index.
           01 realloc-size usage index.
           01 ptr usage pointer.

      * cobl-malloc entry
       PROCEDURE DIVISION using ptr alloc-size.
      * TODO: Check at compile-time if ALLOC is supported.
      * Otherwise, we can try other alloc-like implementations.
         allocate alloc-size characters returning ptr
         exit program.

       entry "cobl-free" using ptr.
         free ptr.
         exit program.

       entry "cobl-realloc" using ptr realloc-size.
