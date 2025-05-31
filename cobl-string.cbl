       IDENTIFICATION DIVISION.
         PROGRAM-ID. string.
       DATA DIVISION.
         working-storage section.
      * NOTE: Be sure to keep these in working-storage. Otherwise, gnucobol
      * will attempt to deallocate the things these point to once the
      * program run ends.
      *
      * TODO: Is this specific to gnucobol or part of the language?
           01 src-char-buffer pic x based.
           01 dst-char-buffer pic x based.

         local-storage section.
           01 char-iter usage index value 0.

           01 src-ptr usage pointer.
           01 dst-ptr usage pointer.

           01 newcap usage binary-c-long unsigned.
           01 tmp-unsigned-long usage binary-c-long unsigned.
           01 c-string usage pointer.
           01 c-string-length usage binary-c-long unsigned.

         LINKAGE SECTION.
           copy "cobl-string-constants.cpy".

           01 local-string.
              copy "cobl-string.cpy".
           01 other-string.
              copy "cobl-string.cpy".

           01 c-string-arg usage pointer.
           01 with-advancing pic x value 'N'.
              88 do-advance value 'Y'.
              88 no-advance value 'N'.

           01 str-length usage binary-c-long unsigned.
           01 str-data usage pointer.
           01 char pic x.
           01 newcap-arg usage binary-c-long unsigned.
           01 newsize-arg usage binary-c-long unsigned.

           01 pic-ptr usage pointer.
           01 pic-length usage index.

           01 idx-arg usage binary-c-long unsigned.
           01 char-return pic x.

      * 0 indicates equal
      * < 0 indicates str1 < str2
      * > 0 indicates str1 > str2
           01 compare-return usage binary-int.

           01 small-pic-str pic x(1024).
           01 small-pic-str-length usage binary-c-long unsigned.

       PROCEDURE DIVISION.
         stop run.

      *
      * Construct a zero-length string.
      *
      * NOTE: This assumes there's existing storage for the string. It will
      * not create new storage for the record representing the string. The
      * only allocations this will do is for the underlying pointer. This is
      * mainly relevant for usage across C API boundaries. The correct way to
      * use this from C is via the "coblang.h" header which provides opaque
      * structs.
      *
       entry "string-construct" using local-string.
         move 0 to cobl-string-length in local-string.
         move cobl-string-default-capacity
              to cobl-string-capacity in local-string.

      * TODO: Check at compile-time if allocate is supported.
      * otherwise, we can try other alloc-like implementations.
         allocate cobl-string-capacity in local-string characters
                  returning cobl-string-ptr in local-string.

         set address of src-char-buffer to
           cobl-string-ptr in local-string.

         move x"00" to src-char-buffer.

      * NOTE: Use `goback` instead of `exit program`. Gnucobol seems to treat
      * calls to these entry points as part of the main program so calls to
      * these functions from C doesn't return from them and continues to the
      * next entry point, rsulting in errors when testing.
         goback.

      *
      * Construct a new string from a PIC-string. Note the string must be
      * able to fit in our buffer.
      *
       entry "string-construct-from-pic-str"
             using local-string small-pic-str small-pic-str-length.
         move address of small-pic-str to c-string.
         move small-pic-str-length to c-string-length.
         perform string-construct-from-c-str.
         goback.

      *
      * Construct a new string from a null-terminated c-style string.
      *
       entry "string-construct-from-c-str"
             using local-string c-string-arg.
         move c-string-arg to c-string.
         move function content-length(c-string) to c-string-length.
       string-construct-from-c-str.
         move c-string-length
              to cobl-string-length in local-string.
         move cobl-string-default-capacity
              to cobl-string-capacity in local-string.

         perform until
                 cobl-string-capacity in local-string >=
                 (cobl-string-length in local-string + 1)
           compute cobl-string-capacity in local-string =
             cobl-string-capacity in local-string * 2
         end-perform.

         allocate cobl-string-capacity in local-string characters
                  returning cobl-string-ptr in local-string.

         call "cobl-memcpy" using cobl-string-ptr in local-string
                                  c-string
                                  cobl-string-length in local-string.

         move cobl-string-ptr in local-string to src-ptr.
         set src-ptr up by cobl-string-length in local-string.
         set address of src-char-buffer to src-ptr.
         move x"00" to src-char-buffer.
       end-string-construct-from-c-str.
         goback.

      *
      * Construct a new string by moving the contents of another string
      * into this one. The old string where data was moved from is
      * effectively destroyed and can be re-used but needs to explicitly
      * be constructed.
      *
       entry "string-construct-move" using local-string other-string.
         move cobl-string-length in other-string to
              cobl-string-length in local-string.
         move cobl-string-capacity in other-string to
              cobl-string-capacity in local-string.
         move cobl-string-ptr in other-string to
              cobl-string-ptr in local-string.

         move null to cobl-string-ptr in other-string.
         move 0 to cobl-string-length in other-string.
         move 0 to cobl-string-capacity in other-string.

         goback.

      *
      * Destroy a string.
      *
       entry "string-destroy" using local-string.
       string-destroy.
         if cobl-string-ptr in local-string not = null
           free cobl-string-ptr in local-string
           move null to cobl-string-ptr in local-string
         end-if.
       end-string-destroy.
         goback.

      *
      * Copy the contents of the other-string into the local-string.
      *
       entry "string-copy" using local-string other-string.
      * TODO: Implement this more efficiently.
         perform string-destroy.
         move cobl-string-ptr in other-string to c-string.
         move function content-length(c-string) to c-string-length.
         perform string-construct-from-c-str.
         goback.

      *
      * Clear a string.
      *
       entry "string-clear" using local-string.
         move 0 to cobl-string-length in local-string.
         set address of src-char-buffer to
             cobl-string-ptr in local-string.
         move x"00" to src-char-buffer.
         goback.

      * 
      * Display a string.
      *
       entry "string-display" using local-string
                                    with-advancing.
         move cobl-string-ptr in local-string to src-ptr.
         move 0 to char-iter.
         perform until char-iter >= cobl-string-length in local-string
           set address of src-char-buffer to src-ptr
           display src-char-buffer with no advancing

           set src-ptr up by 1
           set char-iter up by 1
         end-perform.
         if do-advance
           display " ".

         goback.

      * 
      * Move a string to a cobol string.
      *
       entry "string-copy-to-pic" using local-string pic-ptr pic-length.
         set char-iter to 0.
         move cobl-string-ptr in local-string to src-ptr.
         move pic-ptr to dst-ptr.

      * Clear out the string so we can do normal PIC comparisons with it.
         perform until char-iter >= pic-length
           set address of dst-char-buffer to dst-ptr
           move spaces to dst-char-buffer

           set char-iter up by 1
           set dst-ptr up by 1
         end-perform.
         move pic-ptr to dst-ptr.
         move 0 to char-iter.

         perform until char-iter >= pic-length or
                       char-iter >= cobl-string-length in local-string
           set address of src-char-buffer to src-ptr
           set address of dst-char-buffer to dst-ptr
           move src-char-buffer to dst-char-buffer

           set char-iter up by 1
           set src-ptr up by 1
           set dst-ptr up by 1
         end-perform.

         goback.

      * 
      * Meant to be called by C programs.
      *
       entry "string-length" using local-string str-length.
         move cobl-string-length in local-string to str-length.
         goback.

       entry "string-data" using local-string str-data.
         move cobl-string-ptr in local-string to str-data.
         goback.

       entry "string-front" using local-string char-return.
         set address of src-char-buffer to
             cobl-string-ptr in local-string.
         move src-char-buffer to char-return.
         goback.

       entry "string-back" using local-string char-return.
         move cobl-string-ptr in local-string to src-ptr.
         set src-ptr up by cobl-string-length in local-string.
         set src-ptr down by 1.
         set address of src-char-buffer to src-ptr.
         move src-char-buffer to char-return.
         goback.

       entry "string-at" using local-string idx-arg char-return.
         move cobl-string-ptr in local-string to src-ptr.
         set src-ptr up by idx-arg.
         set address of src-char-buffer to src-ptr.
         move src-char-buffer to char-return.
         goback.

       entry "string-reserve" using local-string newcap-arg.
         move newcap-arg to newcap.
       string-reserve.
         if cobl-string-capacity in local-string < newcap
           allocate newcap characters returning dst-ptr
           call "cobl-memcpy" using dst-ptr
                                    cobl-string-ptr in local-string
                                    cobl-string-length in local-string
           free cobl-string-ptr in local-string
           move dst-ptr to cobl-string-ptr in local-string
           move newcap to cobl-string-capacity in local-string
         end-if.
       end-string-reserve.
         goback.

      *
      * Erase a character in the string.
      *
       entry "string-erase" using local-string idx-arg.
         move cobl-string-ptr in local-string to dst-ptr.
         set dst-ptr up by idx-arg.
         move dst-ptr to src-ptr.
         set src-ptr up by 1.
         move idx-arg to tmp-unsigned-long.
         perform until tmp-unsigned-long >=
                       cobl-string-length in local-string
           set address of src-char-buffer to src-ptr
           set address of dst-char-buffer to dst-ptr

           move src-char-buffer to dst-char-buffer

           set tmp-unsigned-long up by 1
           set src-ptr up by 1
           set dst-ptr up by 1
         end-perform.
         set cobl-string-length in local-string down by 1.
         goback.

      *
      * Resize a string to `newsize-arg`, appending `char` if the new size
      * is greater than the current size.
      *
       entry "string-resize" using local-string newsize-arg char.
         perform string-push-back until
                 cobl-string-length in local-string >= newsize-arg.
         goback.

      *
      * Append a char to the end of the string.
      *
       entry "string-push-back" using local-string char.
       string-push-back.
      * May need to resize.
      * +1 needed for null terminator and +1 needed for the new character..
         perform until cobl-string-capacity in local-string >=
                       cobl-string-length in local-string + 2
           compute newcap = cobl-string-capacity in local-string * 2
           perform string-reserve
         end-perform.

         move cobl-string-ptr in local-string to src-ptr.
         set src-ptr up by cobl-string-length in local-string.
         set address of src-char-buffer to src-ptr.
         move char to src-char-buffer.
         set cobl-string-length in local-string up by 1.

         set src-ptr up by 1.
         set address of src-char-buffer to src-ptr.
         move x"00" to src-char-buffer.

       end-string-push-back.
         goback.

       entry "string-compare-c-string" using local-string c-string-arg
             compare-return.
         move cobl-string-ptr in local-string to src-ptr.
         move c-string-arg to dst-ptr.
         perform forever
           set address of src-char-buffer to src-ptr
           set address of dst-char-buffer to dst-ptr

           if src-char-buffer = x"00" and 
              dst-char-buffer = x"00"
             move 0 to compare-return
             exit perform
           else if src-char-buffer < dst-char-buffer
             move -1 to compare-return
             exit perform
           else if src-char-buffer > dst-char-buffer
             move 1 to compare-return
             exit perform
           end-if

           set src-ptr up by 1
           set dst-ptr up by 1
         end-perform.
         goback.
