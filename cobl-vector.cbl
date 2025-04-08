       IDENTIFICATION DIVISION.
         PROGRAM-ID. vector.
       DATA DIVISION.
         working-storage section.

         local-storage section.
           01 tmp-ptr usage pointer.
           01 tmp-size usage binary-c-long unsigned.
           01 allocation-size-bytes usage binary-c-long unsigned.

           01 newcap usage binary-c-long unsigned.
           01 capacity-bytes usage binary-c-long unsigned.
           01 size-bytes usage binary-c-long unsigned.

         LINKAGE SECTION.
           01 local-vector.
              copy "cobl-vector.cpy".
           01 size-return usage binary-c-long unsigned.
           01 capacity-return usage binary-c-long unsigned.
           01 ptr-return usage pointer.
           01 elem-size-arg usage binary-c-long unsigned.
           01 elem-align-arg usage binary-c-long unsigned.
           01 elem-size-return usage binary-c-long unsigned.
           01 elem-align-return usage binary-c-long unsigned.
           01 newcap-arg usage binary-c-long unsigned.
           01 idx-arg usage binary-c-long unsigned.

       PROCEDURE DIVISION.
         stop run.

       entry "vector-construct"
             using local-vector elem-size-arg elem-align-arg.
         move 0 to vector-size in local-vector.
         move elem-size-arg to vector-elem-size in local-vector.
         move elem-align-arg to vector-elem-align in local-vector.

      * TODO: Define a default vector capacity elsewhere.
         move 128 to vector-capacity in local-vector.

         perform compute-allocation-size.
         compute capacity-bytes =
           allocation-size-bytes * vector-capacity in local-vector.

         allocate capacity-bytes characters
                  returning vector-data in local-vector.

         goback.

       entry "vector-destroy" using local-vector.
         free vector-data in local-vector.
         goback.

       entry "vector-reserve" using local-vector newcap-arg.
         move newcap-arg to newcap.
       vector-reserve.
         if vector-capacity in local-vector >= newcap
           goback.

         perform compute-allocation-size.
         compute capacity-bytes = newcap * allocation-size-bytes.
         compute size-bytes =
           vector-size in local-vector * allocation-size-bytes.

         allocate capacity-bytes characters returning tmp-ptr.
         call "cobl-memcpy" using tmp-ptr
                                  vector-data in local-vector
                                  size-bytes.
         free vector-data in local-vector.
         move tmp-ptr to vector-data in local-vector.
         move newcap to vector-capacity in local-vector.
       end-vector-reserve.
         goback.

      * Allocate more storage to the vector and return a pointer to the new
      * storage so it can be initialized by th
       entry "vector-append-storage" using local-vector ptr-return.
         compute tmp-size = vector-size in local-vector + 1.
         perform until tmp-size <
                       vector-capacity in local-vector
           compute newcap = vector-capacity in local-vector * 2
           perform vector-reserve
         end-perform.

         perform compute-allocation-size.
         move vector-data in local-vector to ptr-return.
         compute size-bytes = 
           allocation-size-bytes * vector-size in local-vector.
         set ptr-return up by size-bytes.
         set vector-size in local-vector up by 1.

         goback.

       entry "vector-at" using local-vector idx-arg ptr-return.
         perform compute-allocation-size.
         move vector-data in local-vector to ptr-return.
         compute size-bytes = allocation-size-bytes * idx-arg.
         set ptr-return up by size-bytes.
         goback.

       entry "vector-size" using local-vector size-return.
         move vector-size in local-vector to size-return.
         goback.

       entry "vector-capacity" using local-vector capacity-return.
         move vector-capacity in local-vector to capacity-return.
         goback.

       entry "vector-data" using local-vector ptr-return.
         move vector-data in local-vector to ptr-return.
         goback.

       entry "vector-elem-size" using local-vector elem-size-return.
         move vector-elem-size in local-vector to elem-size-return.
         goback.

       entry "vector-elem-align" using local-vector elem-align-return.
         move vector-elem-align in local-vector to elem-align-return.
         goback.

       compute-allocation-size.
         compute allocation-size-bytes = function integer(
           (vector-elem-size in local-vector +
            vector-elem-align in local-vector - 1) /
            vector-elem-align in local-vector) *
            vector-elem-align in local-vector.
       end-compute-allocation-size.
