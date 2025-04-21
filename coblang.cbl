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

           01 coblang-lexer.
              copy "lexer.cpy".
           01 coblang-codegen.
              copy "codegen.cpy".

           01 output-file.
              copy "cobl-string.cpy".
           01 input-file.
              copy "cobl-string.cpy".
           01 tmp-string.
              copy "cobl-string.cpy".
           01 idx usage binary-long.

           01 pic-buffer pic x(1024).
           78 pic-buffer-size value length of pic-buffer.

           01 default-output-file-name pic x(100) value "out.obj".

       PROCEDURE DIVISION.
         call "CBL_GC_HOSTED" using argc "argc".

         if argc < 2
           display "Expected at least one argument."
           stop run.

         call "CBL_GC_HOSTED" using argv "argv".

      * arg now points to argv[1].
         set argv up by function byte-length(arg).
         move 1 to idx.

         call "string-construct-from-pic-str" using
              output-file
              default-output-file-name
              function length(
                function trim(default-output-file-name TRAILING)).

         perform until idx >= argc
           set address of arg to argv
           call "string-construct-from-c-str" using tmp-string arg

           call "string-copy-to-pic" using
                tmp-string
                address of pic-buffer
                pic-buffer-size

           evaluate pic-buffer
             when "-o"
               perform handle-output-flag
             when other
               call "string-construct-move" using input-file tmp-string
           end-evaluate

           call "string-destroy" using tmp-string

           set idx up by 1
           set argv up by function byte-length(arg)
         end-perform.

         display "compiling: " no advancing.
         call "string-display" using input-file 'Y'.

         call "lexer-construct" using
              coblang-lexer
              cobl-string-ptr in input-file.

         call "codegen-construct" using
              coblang-codegen
              address of coblang-lexer.
        
         call "codegen-run" using coblang-codegen.

         display "writing to: " no advancing.
         call "string-display" using output-file 'Y'.

         call "write-obj-file" using
              coblang-codegen
              cobl-string-ptr in output-file.

         call "codegen-destroy" using coblang-codegen.
         call "lexer-destroy" using coblang-lexer.
         
         call "string-destroy" using input-file.
         call "string-destroy" using output-file.

         STOP RUN.

       handle-output-flag.
         set idx up by 1.
         if idx >= argc
           display "No argument provided for `-o`"
           stop run.

         set argv up by function byte-length(arg).
         set address of arg to argv.
         call "string-destroy" using output-file.
         call "string-construct-from-c-str" using output-file arg.
       end-handle-output-flag.
