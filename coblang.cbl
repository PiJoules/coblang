       IDENTIFICATION DIVISION.
         PROGRAM-ID. COBLANG-MAIN.
       DATA DIVISION.
         local-STORAGE SECTION.
      * See "CBL_GC_HOSTED".
           01 argc usage binary-long.
      * argv is the char** and we have storage to it.
           01 argv usage pointer.
      * arg will represent a pointer to an element from argv, but we don't
      * allocate any storage to it so we need to point it somewhere into argv
      *
      *   set address of arg to argv
      *
      * This will be needed for incrementing through the characters in an
      * argv[i].
           01 arg usage pointer based.

           01 arg1-string.
             copy "cobl-string.cpy".
           01 coblang-lexer.
             copy "lexer.cpy".
           01 token-string.
             copy "cobl-string.cpy".
           01 coblang-codegen.
             copy "codegen.cpy".

       PROCEDURE DIVISION.
         call "CBL_GC_HOSTED" using argc "argc".

         if argc < 2
           display "Expected at least one argument."
           stop run.

         call "CBL_GC_HOSTED" using argv "argv".

      * arg now points to argv[1].
         set argv up by function byte-length(arg).
         set address of arg to argv.

         call "string-construct-from-c-str" using arg1-string arg.
         display "compiling: " no advancing.
         call "string-display" using arg1-string 'Y'.

         call "lexer-construct" using
              coblang-lexer
              cobl-string-ptr in arg1-string.

         call "codegen-construct" using
              coblang-codegen
              address of coblang-lexer.
        
         call "codegen-run" using coblang-codegen.
         call "write-obj-file" using coblang-codegen.

         call "codegen-destroy" using coblang-codegen.
         call "lexer-destroy" using coblang-lexer.
         
         call "string-destroy" using token-string.
         call "string-destroy" using arg1-string.

         STOP RUN.
