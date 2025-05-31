       IDENTIFICATION DIVISION.
         PROGRAM-ID. expressions recursive.
       DATA DIVISION.
         working-storage section.
           01 expr-vtables-inited pic x value 'N'.

           01 space-expr-vtable.
              copy "expr-vtable.cpy".

           01 string-literal-expr-vtable.
              copy "expr-vtable.cpy".

           01 through-expr-vtable.
              copy "expr-vtable.cpy".

         local-storage section.
           01 tmp-ptr usage pointer.
           01 tmp-char pic x.
           01 tmp-char2 pic x.
           01 tmp-unsigned-int usage binary-int unsigned.
           01 tmp-unsigned-long usage binary-c-long unsigned.

           01 hex-chars pic xx.
           01 hex-as-int usage binary-int unsigned.

           01 expr-storage based.
              copy "expr.cpy".
           01 expr-vtable-storage based.
              copy "expr-vtable.cpy".

         linkage section.
           01 this-expr.
              copy "expr.cpy".
           01 expr-vtable-ptr usage pointer.
           01 line-arg usage binary-int unsigned.
           01 col-arg usage binary-int unsigned.

           01 bool-return pic x.

           01 this-space-expr.
              copy "space-expr.cpy".

           01 this-string-literal-expr.
              copy "string-literal-expr.cpy".
           01 string-arg.
              copy "cobl-string.cpy".

           01 this-through-expr.
              copy "through-expr.cpy".
           01 start-expr-ptr-arg usage pointer.
           01 end-expr-ptr-arg usage pointer.

       procedure division.
         stop run.

       init-expr-vtables.
         if expr-vtables-inited = 'Y'
           exit paragraph.

         set dtor in space-expr-vtable to entry "space-expr-destory".

         set dtor in string-literal-expr-vtable to
             entry "string-literal-expr-destroy".

         set dtor in through-expr-vtable to
             entry "through-expr-destroy".

         move 'Y' to expr-vtables-inited.
       end-init-expr-vtables.

       entry "expr-construct" using this-expr expr-vtable-ptr.
         perform init-expr-vtables.
         move expr-vtable-ptr to vtable-ptr in this-expr.
         move 0 to expr-line in this-expr.
         move 0 to expr-col in this-expr.
         goback.

       entry "space-expr-construct" using this-space-expr.
         set tmp-ptr to address of space-expr-vtable.
         call "expr-construct" using expr in this-space-expr tmp-ptr.
         goback.

       entry "space-expr-destroy" using this-space-expr.
         goback.

       entry "is-space-expr" using this-expr bool-return.
         if vtable-ptr in this-expr = address of space-expr-vtable
           move 'Y' to bool-return
         else
           move 'N' to bool-return
         end-if.
         goback.

       entry "string-literal-expr-construct" using
             this-string-literal-expr string-arg.
         call "expr-construct" using expr in this-string-literal-expr
              address of string-literal-expr-vtable.

         call "string-construct" using str in this-string-literal-expr.
         call "string-copy" using str in this-string-literal-expr
              string-arg.

         goback.

      * This actually constructs the string literal from a hex string,
      * but the ctor name is already long and near the limit.
       entry "string-literal-expr-construct2" using
             this-string-literal-expr string-arg.
         move address of string-literal-expr-vtable to tmp-ptr.
         call "expr-construct" using expr in this-string-literal-expr
              tmp-ptr.

         call "string-front" using string-arg tmp-char.
         if function upper-case(tmp-char) not = "X"
           display "error: hex string does not start with 'x'"
           stop run
         end-if.

         move 1 to tmp-unsigned-long.
         call "string-at" using string-arg tmp-unsigned-long tmp-char.
         if tmp-char not = "'" and tmp-char not = '"'
           display "error: hex string does not start with 'x' followed "
                   "by quotes"
           stop run
         end-if.

         call "string-back" using string-arg tmp-char2.
         if tmp-char2 not = tmp-char
           display "error: hex string does end with starting quote"
           stop run
         end-if.

         call "string-construct" using str in this-string-literal-expr.

         move 2 to tmp-unsigned-long.
         perform until tmp-unsigned-long >=
                       cobl-string-length in string-arg
           call "string-at" using string-arg tmp-unsigned-long
                hex-chars(1:1)
           set tmp-unsigned-long up by 1
           call "string-at" using string-arg tmp-unsigned-long
                hex-chars(2:1)
           set tmp-unsigned-long up by 1

           perform hex-to-int
           set hex-as-int up by 1
           call "string-push-back" using str in this-string-literal-expr
                function char(hex-as-int)
         end-perform.

         goback.

       hex-to-int.
         move 0 to hex-as-int.
         if hex-chars(2:1) >= 'A'
           set hex-as-int up by hex-chars(2:1)
           set hex-as-int down by 'A'
         else
           set hex-as-int up by hex-chars(2:1)
           set hex-as-int down by '0'
         end-if.
         compute hex-as-int = hex-as-int * 16.
         if hex-chars(1:1) >= 'A'
           set hex-as-int up by hex-chars(1:1)
           set hex-as-int down by 'A'
         else
           set hex-as-int up by hex-chars(1:1)
           set hex-as-int down by '0'
         end-if.
       end-hex-to-int.

       entry "string-literal-expr-destroy" using
             this-string-literal-expr.
         call "string-destroy" using str in this-string-literal-expr.
         goback.

       entry "through-expr-construct" using this-through-expr
             start-expr-ptr-arg end-expr-ptr-arg.
         call "expr-construct" using expr in this-through-expr
              address of through-expr-vtable.

         move start-expr-ptr-arg to start-expr-ptr in this-through-expr.
         move end-expr-ptr-arg to end-expr-ptr in this-through-expr.
         goback.

       entry "through-expr-destroy" using this-through-expr.
         set address of expr-storage to
             start-expr-ptr in this-through-expr.
         set address of expr-vtable-storage to
             vtable-ptr in expr-storage.
         call dtor in expr-vtable-storage using expr-storage.
         free start-expr-ptr in this-through-expr.

         set address of expr-storage to
             end-expr-ptr in this-through-expr.
         set address of expr-vtable-storage to
             vtable-ptr in expr-storage.
         call dtor in expr-vtable-storage using expr-storage.
         free end-expr-ptr in this-through-expr.
         goback.
